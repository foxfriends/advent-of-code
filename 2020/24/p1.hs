import Data.Set qualified as Set

toggle set el
  | Set.member el set = Set.delete el set
  | otherwise = Set.insert el set

toCoord coord "" = coord
toCoord (x, y) ('e' : rest) = toCoord (x + 1, y) rest
toCoord (x, y) ('w' : rest) = toCoord (x - 1, y) rest
toCoord (x, y) ('n' : 'e' : rest) = toCoord (x + 1, y - 1) rest
toCoord (x, y) ('n' : 'w' : rest) = toCoord (x, y - 1) rest
toCoord (x, y) ('s' : 'e' : rest) = toCoord (x, y + 1) rest
toCoord (x, y) ('s' : 'w' : rest) = toCoord (x - 1, y + 1) rest

answer = Set.size . foldl toggle Set.empty . fmap (toCoord (0, 0)) . lines

main = getContents >>= print . answer
