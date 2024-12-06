import Control.Monad
import Control.Monad.State
import Data.List
import Data.Set qualified as Set

up = (0, -1)

turn (0, -1) = (1, 0)
turn (1, 0) = (0, 1)
turn (0, 1) = (-1, 0)
turn (-1, 0) = (0, -1)

move (dx, dy) (x, y) = (x + dx, y + dy)

mapsnd f (x, y) = (x, f y)

at x y = gets $ ((!? x) <=< (!? y)) . fst

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

modifyAt 0 f (x : xs) = f x : xs
modifyAt n f (x : xs) = x : modifyAt (n - 1) f xs

answer contents =
  length $ filter (evalState (guardLoops start up)) $ fmap toState $ fmap blockAt $ nub $ evalState (guardWalk start up) (input, undefined)
  where
    toState input = (input, Set.empty)
    blockAt (pos@(x, y))
      | pos /= start = modifyAt y (modifyAt x (const '#')) input
      | otherwise = input

    input = lines contents
    Just startY = findIndex (elem '^') input
    Just startX = elemIndex '^' (input !! startY)
    start = (startX, startY)

main = getContents >>= print . answer
