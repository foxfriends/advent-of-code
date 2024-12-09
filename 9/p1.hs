import Data.Array.Unboxed
import Data.Char

compact input = front 0 (diskmap ! end, end)
  where
    end = length input - 1

    diskmap :: Array Int Int
    diskmap = listArray (0, end) input

    front a (n, b)
      | a == b = replicate n (a `div` 2)
      | a > b = []
      | otherwise = replicate (diskmap ! a) (a `div` 2) ++ back (a + 1) (n, b)

    back a = moveback (diskmap ! a) a
    moveback 0 a r = front (a + 1) r
    moveback i a (0, b) = moveback i a (diskmap ! (b - 2), b - 2)
    moveback i a (n, b)
      | a > b = []
      | otherwise = b `div` 2 : moveback (i - 1) a (n - 1, b)

answer = sum . zipWith (*) [0 ..] . compact . fmap digitToInt . takeWhile isDigit

main = getContents >>= print . answer
