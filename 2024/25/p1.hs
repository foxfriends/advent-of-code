import Data.Either
import Data.List

keyslocks [] = []
keyslocks ("" : rest) = keyslocks rest
keyslocks ("....." : a : b : c : d : e : "#####" : rest) = (Left $ fmap length $ filter (== '#') <$> transpose [a, b, c, d, e]) : keyslocks rest
keyslocks ("#####" : a : b : c : d : e : "....." : rest) = (Right $ fmap length $ filter (== '#') <$> transpose [a, b, c, d, e]) : keyslocks rest

answer = length . filter (all (<= 5) . uncurry (zipWith (+))) . uncurry (liftA2 (,)) . partitionEithers . keyslocks . lines

main = getContents >>= print . answer
