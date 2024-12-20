import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed
import Data.Heap (Entry (Entry), Heap)
import Data.Heap qualified as Heap
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type Grid = Array Pos Char

type Pos = (Int, Int)

type Dijkstra = Control.Monad.State.State DijkstraState

type DijkstraState = (Heap (Entry Int Pos), Map Pos Int)

turn (y, x) = (x, -y)

add2 (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

east = (0, 1)

directions = take 4 $ iterate turn east

reachable :: Grid -> Pos -> Int -> DijkstraState -> DijkstraState
reachable grid pos dist state = foldl visit state $ filter (('#' /=) . (grid !)) $ filter (inRange $ bounds grid) $ add2 pos <$> directions
  where
    visit (queue, dists) pos =
      case dists Map.!? pos of
        Just oldweight | newweight < oldweight -> (nq, nd)
        _ -> (queue, dists)
      where
        newweight = dist + 1
        nq = Heap.insert (Entry newweight pos) queue
        nd = Map.insert pos newweight dists

closest :: Dijkstra (Maybe (Pos, Int))
closest = do
  (q, distances) <- get
  case Heap.uncons q of
    Just (Entry dist pos, queue) -> put (queue, distances) >> return (Just (pos, dist))
    Nothing -> return Nothing

shortestPath :: Grid -> Dijkstra ()
shortestPath grid = do
  next <- closest
  case next of
    Nothing -> return ()
    Just (pos, dist) -> do
      modify (reachable grid pos dist)
      shortestPath grid

readGrid :: String -> Grid
readGrid contents = listArray ((1, 1), (length input, length (input !! 0))) $ concat input
  where
    input = lines contents

indexOf :: (Eq a) => (Ix i) => a -> Array i a -> Maybe i
indexOf v arr = fst <$> find (\x -> snd x == v) (assocs arr)

cheats :: Pos -> [(Pos, Pos)]
cheats pos = [(add2 d pos, add2 d (add2 d pos)) | d <- directions]

answer :: String -> Int
answer contents = length $ do
  from <- indices grid
  (over, to) <- cheats from
  guard $ inRange (bounds grid) to
  guard $ grid ! over == '#'
  guard $ grid ! from /= '#'
  guard $ grid ! to /= '#'
  guard $ savings from to >= 100
  return ()
  where
    grid = readGrid contents
    Just start = indexOf 'S' grid
    Just end = indexOf 'E' grid
    dists = Map.fromList $ (,maxBound) <$> indices grid
    (_, fromStart) = execState (shortestPath grid) (Heap.fromList [Entry 0 start], Map.insert start 0 dists)
    (_, fromEnd) = execState (shortestPath grid) (Heap.fromList [Entry 0 end], Map.insert end 0 dists)
    nocheats = fromStart Map.! end
    savings from to = nocheats - (fromStart Map.! from + fromEnd Map.! to + 2)

main = getContents >>= print . answer
