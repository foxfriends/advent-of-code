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

antinodes :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
antinodes width height nodes = do
  a@(x1, y1) <- nodes
  b@(x2, y2) <- nodes
  guard (a /= b)
  let dx = x2 - x1
      dy = y2 - y1
      g = gcd dx dy
      gx = dx `div` g
      gy = dy `div` g
   in takeWhile (uncurry inGrid) $ [x1, x1 + gx ..] `zip` [y1, y1 + gy ..]
  where
    inGrid x y = inRange (0, width - 1) x && inRange (0, height - 1) y

answer contents = length $ nub $ Map.elems nodes >>= antinodes width height
  where
    input = lines contents
    nodes = readNodes input
    height = length input
    width = length (input !! 0)

main = getContents >>= print . answer
