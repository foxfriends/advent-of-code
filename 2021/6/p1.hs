import Data.Char

parse "\n" = []
parse (',' : rest) = parse rest
parse (n : rest) = digitToInt n : parse rest

update 0 = 6
update n = n - 1

day fish = fmap update fish ++ replicate spawn 8
  where
    spawn = length $ filter (== 0) fish

answer fish = length $ iterate day fish !! 80

main = getContents >>= print . answer . parse
