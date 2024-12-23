import Control.Arrow
import Control.Monad
import Data.Array
import Data.Char
import Data.Graph
import Data.List
import Data.Set qualified as Set
import Data.Tuple
import Debug.Trace

vertex a b = (ord a - ord 'a') * 26 + (ord b - ord 'a')

unvertex v = chr (ord 'a' + v `div` 26) : chr (ord 'a' + v `mod` 26) : ""

edge (a : b : '-' : c : d : "") = (vertex a b, vertex c d)

readGraph input = buildG (0, 26 * 26) (edges ++ fmap swap edges)
  where
    edges = fmap edge $ lines input

isT v = v `div` 26 == ord 't' - ord 'a'

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n - 1) (k - 1) * n `div` k

groups graph from = search 3 graph from
  where
    search 0 graph end = if end == from then [[end]] else []
    search n graph node = do
      next <- graph ! node
      tail <- search (n - 1) graph next
      return $ node : tail

answer input = length $ Set.fromList $ fmap Set.fromList $ do
  from <- filter isT $ vertices graph
  fmap unvertex <$> groups graph from
  where
    graph = readGraph input

main = getContents >>= print . answer
