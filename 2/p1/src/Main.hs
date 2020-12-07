import Data.List.Split

main :: IO ()
main = do
    contents <- getContents
    print $ length [() | line <- lines contents, check line]

check line =
    let len = length [() | ch <- password, ch == head char] in
        low <= len && len <= high
    where
        [criteria, password] = splitOn ": " line
        [range, char] = splitOn " " criteria
        [low, high] = read <$> splitOn "-" range
