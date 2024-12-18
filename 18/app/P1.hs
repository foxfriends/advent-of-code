import Control.Monad.State
import Data.Array.Unboxed
import Data.Heap (Entry (Entry), Heap)
import Data.Heap qualified as Heap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Text.Parsec

type Grid = Array Pos Bool

type Pos = (Int, Int)

type Dijkstra = Control.Monad.State.State DijkstraState

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
  let Just (Entry dist pos, queue) = Heap.uncons q
   in put (queue, distances) >> return (pos, dist)

shortestPath :: Grid -> Pos -> Dijkstra Int
shortestPath grid dest = do
  (pos, dist) <- closest
  if pos == dest
    then return dist
    else do
      modify (reachable grid pos dist)
      shortestPath grid dest

readGrid :: String -> Grid
readGrid contents = accumArray (&&) True ((0, 0), (70, 70)) $ take 1024 ((,False) <$> input)
  where
    Right input = parse (pair `endBy` newline) "" contents

int :: Parsec String s Int
int = read <$> many digit

pair = do
  x <- int
  char ','
  y <- int
  return (x, y)

answer :: String -> Int
answer contents =
  evalState (shortestPath grid (70, 70)) (Heap.fromList [Entry 0 (0, 0)], Map.insert (0, 0) 0 $ Map.fromList $ (,maxBound) <$> indices grid)
  where
    grid = readGrid contents

main = getContents >>= print . answer
