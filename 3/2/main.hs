main :: IO ()
main = do
    contents <- getContents
    let
        levels = lines contents
        a = checkAll [0,3..] levels
        b = checkAll [0..] levels
        c = checkAll [0,5..] levels
        d = checkAll [0,7..] levels
        e = checkAll [0..] [l | (i, l) <- zip [0..] levels, i `mod` 2 == 0]
        in print $ a * b * c * d * e

checkAll :: [Int] -> [String] -> Int
checkAll step levels = length $ filter check $ zip step levels

check :: (Int, String) -> Bool
check (i, line) = line !! (i `mod` length line) == '#'
