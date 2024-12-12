import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Maybe

mapsnd f (x, y) = (x, f y)

type Pos = (Int, Int)
type Grid = Array Pos Char

readGrid :: String -> Grid
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ concat input
  where
    input = lines contents
    height = length input
    width = length (input !! 0)

visit pos = modify' (mapsnd $ Set.insert pos)

been pos = gets (Set.member pos . snd)

at (y, x) = gets ((!? (y, x)) . fst)

neighbours (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

flood :: Pos -> State (Grid, Set Pos) (Maybe (Int, Int))
flood pos = do
  hasbeen <- been pos
  if hasbeen
    then return Nothing
    else do
      ch <- at pos
      case ch of
        Nothing -> return Nothing
        Just ch -> Just <$> flood_ ch pos
  where
    flood_ :: Char -> Pos -> State (Grid, Set Pos) (Int, Int)
    flood_ ch pos = do
      here <- at pos
      hasbeen <- been pos
      if here /= Just ch || hasbeen then return (0, 0) else do
        visit pos
        p <- length . filter (/= Just ch) <$> mapM at (neighbours pos)
        floods <- mapM (flood_ ch) (neighbours pos)
        return $ foldl (\(a1, p1) (a2, p2) -> (a1 + a2, p1 + p2)) (1, p) floods

answer :: String -> Int
answer contents = sum $ fmap (uncurry (*)) $ catMaybes $ evalState (mapM flood (indices grid)) (grid, Set.empty)
  where
    grid = readGrid contents

main = getContents >>= print . answer
