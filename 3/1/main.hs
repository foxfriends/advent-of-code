main :: IO ()
main = do
    contents <- getContents
    print $ length $ filter check $ zip [0,3..] (lines contents)

check :: (Int, String) -> Bool
check (i, line) = line !! (i `mod` length line) == '#'
