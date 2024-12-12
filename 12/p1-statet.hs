import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed
import Data.Set qualified as Set
import Data.Set (Set)
import Debug.Trace
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

at :: Monad m => Pos -> StateT (Grid, u) m (Maybe Char)
at (y, x) = gets ((!? (y, x)) . fst)

neighbours (y, x) = [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]

flood :: Pos -> StateT (Grid, Set Pos) [] (Int, Int)
flood pos = do
  hasbeen <- been pos
  guard $ not hasbeen
  ch <- at pos
  ch <- lift $ maybeToList ch
  traceShowId <$> flood_ ch pos
  where
    flood_ :: Char -> Pos -> StateT (Grid, Set Pos) [] [(Int, Int)]
    flood_ ch pos = do
      here <- at pos
      hasbeen <- been pos
      guard $ not hasbeen
      guard (here == Just ch)
      visit pos
      ns <- mapM at (neighbours pos)
      p <- pure $ length (filter (/= Just ch) ns)
      floods <- fmap (flood_ ch) (neighbours pos)
      return (1, p)

answer contents = sum $ fmap (uncurry (*)) $ evalStateT (msum $ fmap flood $ indices grid) (grid, Set.empty)
  where
    grid = readGrid contents

main = getContents >>= print . answer
