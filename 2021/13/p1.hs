import Control.Arrow
import Data.Function
import Data.List
import Data.Array.Unboxed
import Text.Parsec

int :: Parsec String u Int
int = read <$> many1 digit

point :: Parsec String u (Int, Int)
point = do
  [x, y] <- int `sepBy` char ','
  return (y, x)

fold = do
  string "fold along "
  axis <- oneOf "xy"
  char '='
  pos <- int
  return (axis, pos)

input = do
  points <- makeGrid <$> (point `endBy` newline)
  newline
  folds <- fold `endBy` newline
  return (points, folds)
  where
    makeGrid =
      fmap (fmap snd)
        . groupBy ((==) `on` (fst . fst))
        . assocs
        . makeArray
    makeArray :: [(Int, Int)] -> Array (Int, Int) Bool
    makeArray points = accumArray (||) False ((0, 0), maxPoint points)
        $ fmap (, True) points
    maxPoint = (maximum *** maximum) . unzip

zipWithLong _ [] rr = rr
zipWithLong _ ll [] = ll
zipWithLong f (l:ll) (r:rr) = f l r : zipWithLong f ll rr

applyFold 'x' pos grid = transpose $ applyFold 'y' pos $ transpose grid
applyFold 'y' pos grid = reverse $ zipWithLong (zipWith (||)) (reverse above) below
  where (above, _:below) = splitAt pos grid

answer contents = length $ filter id $ concat $ uncurry applyFold (head folds) points
  where
    Right (points, folds) = parse input "" contents

main = getContents >>= print . answer
