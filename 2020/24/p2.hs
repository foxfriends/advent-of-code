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

neighbours (x, y) = [(x - 1, y), (x + 1, y), (x + 1, y - 1), (x, y - 1), (x, y + 1), (x - 1, y + 1)]

auto set = Set.union stillblack nowblack
  where
    stillblack = Set.filter ((== 1) . length . filter (flip Set.member set) . neighbours) set
    nowblack =  Set.filter ((== 2) . length . filter (flip Set.member set) . neighbours) $ Set.fromList $ concat $ fmap neighbours $ Set.elems set

answer input = Set.size $ life !! 100
  where
    life = iterate auto $ foldl toggle Set.empty $ fmap (toCoord (0, 0)) $ lines input

main = getContents >>= print . answer
