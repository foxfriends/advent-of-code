import Control.Monad
import Control.Monad.State
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

at y x = gets ((!? (y, x)) . fst)

mapsnd f (x, y) = (x, f y)

visit point = modify (mapsnd $ Set.insert point)

been point = gets $ elem point . snd

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

run pos dir = do
  predict <- uncurry at (move pos dir)
  case predict of
    Just '#' -> return pos
    Nothing -> return pos
    Just _ -> run dest dir
  where
    dest = move pos dir

guardLoops pos dir = do
  hasBeen <- been (pos, dir)
  visit (pos, dir)
  if hasBeen
    then return True
    else do
      end <- run pos dir
      dest <- next end dir
      case dest of
        Nothing -> return False
        Just (newpos, newdir) -> guardLoops newpos newdir

answer contents =
  length $ filter (evalState (guardLoops start up)) $ fmap (toState . blockAt) $ Set.toList $ Set.fromList $ evalState (guardWalk start up) (grid, undefined)
  where
    toState input = (input, Set.empty)

    grid = readGrid contents
    Just (start, _) = find (('^' ==) . snd) $ assocs grid

    blockAt pos
      | pos /= start = grid // [(pos, '#')]
      | otherwise = grid

main = getContents >>= print . answer
