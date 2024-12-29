import Control.Monad.State
import Data.Array.Unboxed
import Data.Function
import Data.Heap (Entry (Entry), Heap)
import Data.Heap qualified as Heap
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

type Grid = Array Pos Char

type Maze = Map (Pos, Dir) [(Pos, Dir, Int, Set Pos)]

type Pos = (Int, Int)

type Dir = (Int, Int)

type Dijkstra = State DijkstraState

type DijkstraState = (Heap (Entry Int (Pos, Dir)), Map (Pos, Dir) (Int, Set Pos))

turn (y, x) = (x, -y)

unturn (y, x) = (-x, y)

add2 (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

east = (0, 1)

south = turn east

west = turn south

north = turn west

directions = take 4 $ iterate turn east

reachable :: Maze -> Pos -> Dir -> Int -> Set Pos -> DijkstraState -> DijkstraState
reachable maze pos dir dist path state = foldl visit state $ maze Map.! (pos, dir)
  where
    visit (queue, dists) (pos, dir, weight, newpath) =
      case dists Map.!? (pos, dir) of
        Just (oldweight, _) | newweight < oldweight -> (nq, Map.insert (pos, dir) (newweight, np) dists)
        Just (oldweight, paths) | newweight == oldweight -> (nq, Map.insert (pos, dir) (newweight, Set.union paths np) dists)
        _ -> (queue, dists)
      where
        newweight = weight + dist
        nq = Heap.insert (Entry newweight (pos, dir)) queue
        np = Set.union path newpath

closest :: Dijkstra (Pos, Dir, Int, Set Pos)
closest = do
  (q, distances) <- get
  let Just (Entry dist (pos, dir), queue) = Heap.uncons q
   in put (queue, distances) >> return (pos, dir, dist, snd (distances Map.! (pos, dir)))

shortestPath :: Maze -> Pos -> Dijkstra Int
shortestPath maze dest = do
  (pos, dir, dist, path) <- closest
  if pos == dest
    then return $ length path
    else do
      modify (reachable maze pos dir dist path)
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
    transturn pos dir = ((pos, dir), [(pos, turn dir, 1000, Set.empty), (pos, unturn dir, 1000, Set.empty)])
    transrun pos dir = do
      (end, dist, path) <- toEnd (add2 pos dir)
      return ((pos, dir), [(end, dir, dist, path)])
      where
        toEnd pos
          | not $ isOpen pos = Nothing
          | isCorner pos = Just (pos, 1, Set.singleton pos)
          | otherwise = Just $ maybe (pos, 1, Set.singleton pos) collect (toEnd (add2 pos dir))
          where
            collect (end, dist, path) = (end, dist + 1, Set.insert pos path)

answer :: String -> Int
answer contents =
  evalState (shortestPath maze end) (Heap.fromList [Entry 0 (start, east)], Map.insert (start, east) (0, Set.singleton start) $ Map.fromList $ (,(maxBound, Set.empty)) <$> nodes)
  where
    grid = readGrid contents
    maze = toMaze grid
    nodes = Map.keys maze
    Just start = indexOf 'S' grid
    Just end = indexOf 'E' grid

main = getContents >>= print . answer
