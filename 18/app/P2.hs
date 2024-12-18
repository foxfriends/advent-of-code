import Control.Monad.State
import Data.Array.Unboxed
import Data.Heap (Entry (Entry), Heap)
import Data.Heap qualified as Heap
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.Parsec

type Grid = Array Pos Bool

type Pos = (Int, Int)

type Dijkstra = StateT DijkstraState Maybe

type DijkstraState = (Heap (Entry Int Pos), Map Pos Int)

turn (y, x) = (x, -y)

add2 (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

east = (0, 1)

directions = take 4 $ iterate turn east

reachable :: Grid -> Pos -> Int -> DijkstraState -> DijkstraState
reachable grid pos dist state = foldl visit state $ filter (grid !) $ filter (inRange $ bounds grid) $ add2 pos <$> directions
  where
    visit (queue, dists) pos =
      case dists Map.!? pos of
        Just oldweight | newweight < oldweight -> (nq, nd)
        _ -> (queue, dists)
      where
        newweight = dist + 1
        nq = Heap.insert (Entry newweight pos) queue
        nd = Map.insert pos newweight dists

closest :: Dijkstra (Pos, Int)
closest = do
  (q, distances) <- get
  case Heap.uncons q of
    Just (Entry dist pos, queue) -> put (queue, distances) >> return (pos, dist)
    Nothing -> lift Nothing

shortestPath :: Grid -> Pos -> Dijkstra Int
shortestPath grid dest = do
  (pos, dist) <- closest
  if pos == dest
    then return dist
    else do
      modify (reachable grid pos dist)
      shortestPath grid dest

size = (70, 70)

readGrid :: [Pos] -> Grid
readGrid input = accumArray (&&) True ((0, 0), size) $ (,False) <$> input

int :: Parsec String s Int
int = read <$> many digit

pair = do
  x <- int
  char ','
  y <- int
  return (x, y)

check :: Grid -> Maybe Int
check grid = evalStateT (shortestPath grid size) (Heap.fromList [Entry 0 (0, 0)], Map.insert (0, 0) 0 $ Map.fromList $ (,maxBound) <$> indices grid)

answer contents = last ans
  where
    Just ans = find ((== Nothing) . check . readGrid) $ flip take input <$> [1024 ..]
    Right input = parse (pair `endBy` newline) "" contents

main = getContents >>= print . answer
