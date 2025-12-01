rotate (count, rot) ('L' : dist) = rotate_ count rot (-read dist)
rotate (count, rot) ('R' : dist) = rotate_ count rot (read dist)

rotate_ count rot dist = (count2, rot2)
  where
    rotations = abs dist `div` 100
    crossed = not $ (rot + dist `rem` 100) `elem` [0 .. 99]
    count2 = count + rotations + if crossed then 1 else 0
    rot2 = (rot + (dist `rem` 100) + 100) `rem` 100

main = getContents >>= print . fst . foldl rotate (0, 50) . lines
