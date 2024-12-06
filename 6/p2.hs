import Control.Monad
import Control.Monad.State
import Data.Function
import Data.List
import Data.Set (Set)
import Data.Set qualified as Set
import Debug.Trace

up = (0, -1)

turn (0, -1) = (1, 0)
turn (1, 0) = (0, 1)
turn (0, 1) = (-1, 0)
turn (-1, 0) = (0, -1)

unturn (1, 0) = (0, -1)
unturn (0, 1) = (1, 0)
unturn (-1, 0) = (0, 1)
unturn (0, -1) = (-1, 0)

move (dx, dy) (x, y) = (x + dx, y + dy)

runToEnd pos dir = do
  atpos <- uncurry at pos
  case atpos of
    Nothing -> return []
    Just '#' -> return []
    _ -> (pos :) <$> runToEnd (move dir pos) dir

at :: Int -> Int -> State ([String], Set ((Int, Int), (Int, Int))) (Maybe Char)
at x y = gets $ ((!? x) <=< (!? y)) . fst

watch dir poss = do
  (map, watching) <- get
  put (map, foldl (flip Set.insert) watching $ fmap (flip (,) dir) poss)

next pos dir = do
  predict <- uncurry at (move pos dir)
  case predict of
    Just '#' -> do
      next pos (turn dir)
    Just _ -> return $ Just (move pos dir, dir)
    Nothing -> return Nothing

guardMove pos dir '#' = let newdir = turn dir in (move newdir pos, newdir)
guardMove pos dir _ = (move dir pos, dir)

guardWalk :: (Int, Int) -> (Int, Int) -> State ([String], Set ((Int, Int), (Int, Int))) Int
guardWalk pos dir = do
  watching <- gets (snd)
  watchFront <- runToEnd pos dir
  watchBack <- runToEnd pos (turn $ turn dir)
  watch (unturn dir) (watchFront ++ watchBack)
  path <- runToEnd pos dir
  let end = last path
      candidates = traceShowId $ filter (\a -> (a, dir) `elem` watching) path
   in do
        dest <- next (last path) dir
        case dest of
          Nothing -> return $ length candidates
          Just (newpos, newdir) -> do
            rest <- guardWalk newpos newdir
            return $ length candidates + rest

answer contents = evalState (guardWalk (startX, startY) up) (input, Set.empty)
  where
    input = lines contents
    Just startY = findIndex (elem '^') input
    Just startX = elemIndex '^' (input !! startY)

main = getContents >>= print . answer
