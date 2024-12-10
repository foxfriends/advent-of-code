import Data.Array.Unboxed
import Data.Char
import Data.Function
import Data.Set qualified as Set

readGrid :: String -> Array (Int, Int) Int
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ digitToInt <$> concat input
  where
    input = lines contents
    height = length input
    width = length (input !! 0)

neighbours (y, x) = [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)]

score grid = reachable 0 . Set.singleton
  where
    reachable 9 pos = length pos
    reachable n pos = reachable (n + 1) next
      where
        next =
          Set.toList pos
            >>= neighbours
            & Set.fromList
            & Set.filter (inRange $ bounds grid)
            & Set.filter ((==) (n + 1) . (!) grid)

answer contents = sum $ score input <$> filter ((==) 0 . (!) input) (indices input)
  where
    input = readGrid contents

main = getContents >>= print . answer
