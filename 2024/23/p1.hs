import Data.Array
import Data.Char
import Data.Graph
import Data.Set (fromList)
import Data.Tuple

vertex a b = (ord a - ord 'a') * 26 + (ord b - ord 'a')

edge (a : b : '-' : c : d : "") = (vertex a b, vertex c d)

readGraph input = buildG (0, 26 * 26) (edges ++ fmap swap edges)
  where
    edges = fmap edge $ lines input

isT v = v `div` 26 == ord 't' - ord 'a'

groups graph from = search 3 graph from
  where
    search 0 graph end = if end == from then [[]] else []
    search n graph node = do
      next <- graph ! node
      tail <- search (n - 1) graph next
      return $ node : tail

answer input = length $ fromList $ fmap fromList $ do
  from <- filter isT $ vertices graph
  groups graph from
  where
    graph = readGraph input

main = getContents >>= print . answer
