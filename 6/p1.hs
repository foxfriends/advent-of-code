import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Set qualified as Set

up = (0, -1)

turn (0, -1) = (1, 0)
turn (1, 0) = (0, 1)
turn (0, 1) = (-1, 0)
turn (-1, 0) = (0, -1)

move (dx, dy) (x, y) = (x + dx, y + dy)

at x y = reader ((!? x) <=< (!? y))

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

answer contents = length $ Set.fromList $ runReader (guardWalk (startX, startY) up) input
  where
    input = lines contents
    Just startY = findIndex (elem '^') input
    Just startX = elemIndex '^' (input !! startY)

main = getContents >>= print . answer
