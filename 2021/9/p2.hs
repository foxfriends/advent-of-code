import Data.Array.Unboxed
import Data.Char
import Data.Maybe
import Data.List (sortBy)
import Data.Set qualified as Set

readGrid :: String -> Array (Int, Int) Int
readGrid contents = listArray ((1, 1), (height, width)) $ concat input
  where
    input = fmap (fmap digitToInt) $ lines contents
    width = length $ input !! 0
    height = length input

neighbours (y, x) = [(y - 1, x), (y + 1, x), (y, x + 1), (y, x - 1)]

isMinPoint grid here = all (> (grid ! here)) $ mapMaybe (grid !?) (neighbours here)

basin grid origin = basin_ Set.empty origin
  where
    basin_ :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
    basin_ co here
      | not $ inRange (bounds grid) here = co
      | grid ! here == 9 = co
      | Set.member here co = co
      | otherwise = foldl basin_ (Set.insert here co) (neighbours here)

answer grid = product $ take 3 $ sortBy (flip compare) $ fmap (Set.size . basin grid) $ filter (isMinPoint grid) $ indices grid

main = getContents >>= print . answer . readGrid
