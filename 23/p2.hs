import Data.Array
import Data.Char
import Data.Graph
import Data.List
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple

vertex a b = (ord a - ord 'a') * 26 + (ord b - ord 'a')

unvertex a = (chr (a `div` 26 + ord 'a')) : (chr (a `mod` 26 + ord 'a')) : ""

edge (a : b : '-' : c : d : "") = (vertex a b, vertex c d)

readGraph input = buildG (0, 26 * 26) (edges ++ fmap swap edges)
  where
    edges = fmap edge $ lines input

cliquesize graph from =
  if null neighbours
    then 0
    else length $ filter (== 11) $ do
      n <- neighbours
      return $ length $ Set.intersection (Set.fromList neighbours) (Set.fromList (graph ! n))
  where
    neighbours = graph ! from

answer input = intercalate "," $ fmap unvertex $ sort $ Map.keys $ Map.filter (== 12) $ Map.fromList $ do
  from <- vertices graph
  return (from, cliquesize graph from)
  where
    graph = readGraph input

main = getContents >>= putStrLn . answer
