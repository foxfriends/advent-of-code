import Control.Monad
import Control.Monad.Reader
import Data.Array.Unboxed
import Data.List (find)
import Data.Set qualified as Set

readGrid :: String -> Array (Int, Int) Char
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ concat input
  where
    input = lines contents
    height = length input
    width = length (input !! 0)

up = (-1, 0)

turn (y, x) = (x, -y)

move (y, x) (dy, dx) = (y + dy, x + dx)

at y x = reader (!? (y, x))

next pos dir = do
  predict <- uncurry at (move pos dir)
  case predict of
    Just '#' -> next pos (turn dir)
    Just _ -> return $ Just (move pos dir, dir)
    Nothing -> return Nothing

guardMove pos dir '#' = let newdir = turn dir in (move newdir pos, newdir)
guardMove pos dir _ = (move dir pos, dir)

guardWalk pos dir = do
  dest <- next pos dir
  case dest of
    Nothing -> return [pos]
    Just (newpos, newdir) -> do
      rest <- guardWalk newpos newdir
      return (pos : rest)

answer contents = length $ Set.fromList $ runReader (guardWalk start up) grid
  where
    grid = readGrid contents
    Just (start, _) = find (('^' ==) . snd) $ assocs grid

main = getContents >>= print . answer
