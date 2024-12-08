import Control.Monad
import Data.Ix
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

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

answer contents = length $ filter (uncurry inGrid) $ nub $ Map.elems nodes >>= antinodes
  where
    input = lines contents
    nodes = readNodes input
    height = length input
    width = length (input !! 0)
    inGrid x y = inRange (0, width - 1) x && inRange (0, height - 1) y

main = getContents >>= print . answer
