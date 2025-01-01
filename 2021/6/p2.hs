import Data.Char
import Data.Array.Unboxed

parse "\n" = []
parse (',' : rest) = parse rest
parse (n : rest) = digitToInt n : parse rest

count fish = accumArray (+) 0 (0, 8) ((, 1) <$> fish)

day :: Array Int Int -> Array Int Int
day fish =
  listArray (0, 8) [
    fish ! 1,
    fish ! 2,
    fish ! 3,
    fish ! 4,
    fish ! 5,
    fish ! 6,
    fish ! 7 + fish ! 0,
    fish ! 8,
    fish ! 0
  ]

answer fish = sum $ iterate day (count fish) !! 256

main = getContents >>= print . answer . parse
