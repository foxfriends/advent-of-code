rotate rot ('L' : dist) = (rot - (read dist `mod` 100) + 100) `mod` 100
rotate rot ('R' : dist) = (rot + read dist) `mod` 100

main = getContents >>= print . length . filter ((==) 0) . scanl rotate 50 . lines
