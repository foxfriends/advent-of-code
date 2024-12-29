import Control.Monad
import Data.Ix
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

readNodes :: [String] -> Map Char [(Int, Int)]
readNodes input = Map.fromListWith (++) $ do
  (y, line) <- [0 ..] `zip` input
  (x, char) <- [0 ..] `zip` line
  guard (char /= '.')
  return (char, [(x, y)])

antinodes :: [(Int, Int)] -> [(Int, Int)]
antinodes nodes = do
  a@(x1, y1) <- nodes
  b@(x2, y2) <- nodes
  guard (a /= b)
  let x3 = 2 * x2 - x1
      y3 = 2 * y2 - y1
   in return (x3, y3)

answer contents = length $ filter inGrid $ Set.toList $ Set.fromList $ Map.elems nodes >>= antinodes
  where
    input = lines contents
    nodes = readNodes input
    height = length input
    width = length (input !! 0)
    inGrid = inRange ((0, 0), (width - 1, height - 1))

main = getContents >>= print . answer
