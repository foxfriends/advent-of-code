import Data.List
import Data.Ord

split :: Eq a => a -> [a] -> [[a]]
split x ys = splitInner x [] ys
    where
        splitInner _ [] [] = []
        splitInner _ r [] = [reverse r]
        splitInner x r (y : ys)
            | x == y = reverse r : splitInner x [] ys
            | otherwise = splitInner x (y : r) ys

main :: IO ()
main = do
    [arrival, buses] <- lines <$> getContents
    let
        start = read arrival
        intervals = [read n | n <- split ',' buses, n /= "x"] :: [Int]
        waits = [(n, n - start `mod` n) | n <- intervals]
        in print . uncurry (*) $ minimumBy (comparing snd) waits
