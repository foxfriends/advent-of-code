import Data.Char
import Data.List

rotate (x:xs) = xs ++ [x]

wrapdown 1 = 9
wrapdown n = n - 1

move (x:xs) = (x : pref) ++ (t : rm) ++ suf
  where
    (rm, rest) = splitAt 3 xs
    Just destination = find (`elem` rest) $ tail $ iterate wrapdown x
    (pref, t:suf) = span (/= destination) rest

ans (1:v) = fmap intToDigit v
ans v = ans $ rotate v

answer input = ans $ iterate (rotate . move) input !! 100

main = getContents >>= putStrLn . answer . fmap digitToInt . init
