{-# LANGUAGE ViewPatterns #-}
import Data.List

main :: IO ()
main = do
    contents <- getContents
    print $ checkAll $ lines contents

checkAll :: [String] -> Int
checkAll [] = 0
checkAll ls =
    if length (filter valid (passport >>= words)) == 7
    then 1 + checkAll rest
    else checkAll rest
    where (passport, rest) = case findIndex null ls of
            Nothing -> (ls, [])
            Just n -> let (p, r) = splitAt n ls in (p, tail r)

valid :: String -> Bool
valid (stripPrefix "byr:" -> Just y) = let year = read y in year >= 1920 && year <= 2002
valid (stripPrefix "iyr:" -> Just y) = let year = read y in year >= 2010 && year <= 2020
valid (stripPrefix "eyr:" -> Just y) = let year = read y in year >= 2020 && year <= 2030
valid (stripPrefix "hgt:" -> Just str)
    | "cm" `isSuffixOf` str = height >= 150 && height <= 193
    | "in" `isSuffixOf` str = height >= 59 && height <= 76
    | otherwise = False
    where
        height :: Int
        height = read (take (length str - 2) str)
valid (stripPrefix "hcl:#" -> Just col) | length col == 6 = all isHex col
valid (stripPrefix "ecl:" -> Just col) | col `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] = True
valid (stripPrefix "pid:" -> Just pid) | length pid == 9 = all (`elem` ['0'..'9']) pid
valid _ = False

isHex :: Char -> Bool
isHex ch = ch `elem` ['0'..'9'] ++ ['a'..'f']
