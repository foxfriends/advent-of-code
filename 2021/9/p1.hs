import Data.Array.Unboxed
import Data.Char
import Data.Maybe

readGrid :: String -> Array (Int, Int) Int
readGrid contents = listArray ((1, 1), (height, width)) $ concat input
  where
    input = fmap (fmap digitToInt) $ lines contents
    width = length $ input !! 0
    height = length input

neighbours (y, x) = [(y - 1, x), (y + 1, x), (y, x + 1), (y, x - 1)]

isMinPoint grid here = all (> (grid ! here)) $ mapMaybe (grid !?) (neighbours here)

answer grid = sum $ fmap ((+ 1) . (grid !)) $ filter (isMinPoint grid) $ indices grid

main = getContents >>= print . answer . readGrid
