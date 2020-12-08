import Data.List.Split

main :: IO ()
main = do
    contents <- getContents
    print $ length [() | line <- lines contents, check line]

check line = (isLeft && not isRight) || (isRight && not isLeft)
    where
        [criteria, password] = splitOn ": " line
        [positions, char] = splitOn " " criteria
        [left, right] = read <$> splitOn "-" positions
        isLeft = (password !! (left - 1)) == head char
        isRight = (password !! (right - 1)) == head char
