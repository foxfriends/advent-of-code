main = getContents >>= print . length . filter ((`elem` [2,3,4,7]) . length) . concat . fmap (drop 11 . words) . lines
