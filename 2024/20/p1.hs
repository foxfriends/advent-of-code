import Control.Monad
import Data.Array.Unboxed
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

turn (y, x) = (x, -y)

add2 (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)

directions = take 4 $ iterate turn (0, 1)

readPath contents = start : next start start
  where
    input = lines contents
    grid = listArray ((1, 1), (length input, length (input !! 0))) $ concat input
    Just start = indexOf 'S' grid
    next prev curr = case find (\p -> grid ! p /= '#') $ filter (/= prev) $ add2 curr <$> directions of
      Nothing -> []
      Just n -> n : next curr n

indexOf :: (Eq a) => (Ix i) => a -> Array i a -> Maybe i
indexOf v arr = fst <$> find (\x -> snd x == v) (assocs arr)

cheats pos = [add2 d $ add2 d pos | d <- directions]

answer contents = length $ do
  from <- path
  to <- cheats from
  guard $ Map.member to distance
  guard $ savings from to >= 100
  return ()
  where
    path = readPath contents
    distance = Map.fromList $ path `zip` [0..]
    savings from to = distance Map.! to - distance Map.! from - 2

main = getContents >>= print . answer
