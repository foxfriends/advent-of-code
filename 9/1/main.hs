check :: [Int] -> [Int] -> Int
check _ [] = error "invalid"
check preamble (x : xs) = if x `elem` valid preamble then check (x : init preamble) xs else x

valid :: [Int] -> [Int]
valid preamble = [(preamble !! i) + (preamble !! j) | i <- [0..23], j <- [i+1..24]]

main :: IO ()
main = do
    numbers <- fmap read . lines <$> getContents :: IO [Int]
    let (preamble, contents) = splitAt 25 numbers in
        print $ check (reverse preamble) contents
