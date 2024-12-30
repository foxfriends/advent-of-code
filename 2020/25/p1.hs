import Data.List

transform s = iterate (\v -> (v * s) `mod` 20201227) 1

answer :: [Int] -> Int
answer [a, b] = transform b !! loopsize
  where
    Just loopsize = findIndex (== a) $ transform 7

main = getContents >>= print . answer . fmap read . lines
