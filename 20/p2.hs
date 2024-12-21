import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import Data.Array.Unboxed
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

turn (y, x) = (x, -y)

add2 (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

directions = take 4 $ iterate turn (0, 1)

readPath grid = start : next start start
  where
    Just start = indexOf 'S' grid
    next prev curr = case find (\p -> grid ! p /= '#') $ filter (/= prev) $ add2 curr <$> directions of
      Nothing -> []
      Just n -> n : next curr n

readGrid contents = listArray ((1, 1), (length input, length (input !! 0))) $ concat input
  where input = lines contents

indexOf :: (Eq a) => (Ix i) => a -> Array i a -> Maybe i
indexOf v arr = fst <$> find (\x -> snd x == v) (assocs arr)

manhattan (y1, x1) (y2, x2) = abs (y2 - y1) + abs (x2 - x1)

answer contents = length $ do
  from :| rest <- tails1 path
  to <- filter (\to -> manhattan from to <= 20) rest
  guard $ savings from to >= 100
  return ()
  where
    grid = readGrid contents
    path = readPath grid
    distance = Map.fromList $ path `zip` [0..]
    savings from to = distance Map.! to - distance Map.! from - manhattan from to

main = getContents >>= print . answer
