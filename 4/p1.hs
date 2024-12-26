import Data.List

main :: IO ()
main = do
    contents <- getContents
    print $ checkAll $ lines contents

checkAll :: [String] -> Int
checkAll [] = 0
checkAll ls =
    if length [w | w <- fmap (take 3) (passport >>= words), w `elem` ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]] == 7
    then 1 + checkAll rest
    else checkAll rest
    where (passport, rest) = case findIndex null ls of
            Nothing -> (ls, [])
            Just n -> let (p, r) = splitAt n ls in (p, tail r)
