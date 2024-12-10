import Control.Monad
import Data.Array.Unboxed
import Data.Char

readGrid :: String -> Array (Int, Int) Int
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ digitToInt <$> concat input
  where
    input = lines contents
    height = length input
    width = length (input !! 0)

neighbours (y, x) = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

score :: Array (Int, Int) Int -> (Int, Int) -> Int
score grid = sum . paths 0
  where
    paths 9 pos = [1]
    paths n pos = do
      next <- neighbours pos
      guard $ inRange (bounds grid) next
      guard $ grid ! next == n + 1
      paths (n + 1) next

answer contents = sum $ score input <$> filter ((==) 0 . (!) input) (indices input)
  where
    input = readGrid contents

main = getContents >>= print . answer
