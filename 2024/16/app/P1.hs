import Control.Monad.State
import Data.Array.Unboxed
import Data.Bifunctor
import Data.Function
import Data.Heap (Entry (Entry), Heap)
import Data.Heap qualified as Heap
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe

type Grid = Array Pos Char

type Maze = Map (Pos, Dir) [(Pos, Dir, Int)]

type Pos = (Int, Int)

type Dir = (Int, Int)

type Dijkstra = State DijkstraState

type DijkstraState = (Heap (Entry Int (Pos, Dir)), Map (Pos, Dir) Int)

turn (y, x) = (x, -y)

unturn (y, x) = (-x, y)

add2 (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

east = (0, 1)

south = turn east

west = turn south

north = turn west

directions = take 4 $ iterate turn east

reachable :: Maze -> Pos -> Dir -> Int -> DijkstraState -> DijkstraState
reachable maze pos dir dist state = foldl visit state $ maze Map.! (pos, dir)
  where
    visit (queue, dists) (pos, dir, weight) =
      if Just newweight < oldweight then (nq, nd) else (queue, dists)
      where
        oldweight = dists Map.!? (pos, dir)
        newweight = weight + dist
        nq = Heap.insert (Entry newweight (pos, dir)) queue
        nd = Map.insert (pos, dir) newweight dists

closest :: Dijkstra (Pos, Dir, Int)
closest = do
  (q, distances) <- get
  let Just (Entry dist (pos, dir), queue) = Heap.uncons q
   in put (queue, distances) >> return (pos, dir, dist)

shortestPath :: Maze -> Pos -> Dijkstra Int
shortestPath maze dest = do
  (pos, dir, dist) <- closest
  if pos == dest
    then return dist
    else do
      modify (reachable maze pos dir dist)
      shortestPath maze dest

readGrid :: String -> Grid
readGrid contents = listArray ((0, 0), (length input - 1, length (input !! 0) - 1)) $ concat input
  where
    input = lines contents

indexOf :: (Eq a) => (Ix i) => a -> Array i a -> Maybe i
indexOf v arr = fst <$> find (\x -> snd x == v) (assocs arr)

toMaze :: Grid -> Maze
toMaze grid =
  indices grid
    & filter isOpen
    & filter isCorner
    >>= transitions
    & Map.fromListWith (++)
  where
    isOpen pos = grid ! pos /= '#'
    isCorner pos = (isOpen (add2 pos east) || isOpen (add2 pos west)) && (isOpen (add2 pos north) || isOpen (add2 pos south))
    transitions pos = fmap (transturn pos) directions ++ mapMaybe (transrun pos) directions
    transturn pos dir = ((pos, dir), [(pos, turn dir, 1000), (pos, unturn dir, 1000)])
    transrun pos dir = do
      (end, dist) <- toEnd (add2 pos dir)
      return ((pos, dir), [(end, dir, dist)])
      where
        toEnd pos
          | not $ isOpen pos = Nothing
          | isCorner pos = Just (pos, 1)
          | otherwise = Just $ maybe (pos, 1) (second (+ 1)) (toEnd (add2 pos dir))

answer :: String -> Int
answer contents =
  evalState (shortestPath maze end) (Heap.fromList [Entry 0 (start, east)], Map.insert (start, east) 0 $ Map.fromList $ (,maxBound) <$> nodes)
  where
    grid = readGrid contents
    maze = toMaze grid
    nodes = Map.keys maze
    Just start = indexOf 'S' grid
    Just end = indexOf 'E' grid

main = getContents >>= print . answer
