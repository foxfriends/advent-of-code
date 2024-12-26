import Data.List

differences :: [Int] -> [Int]
differences (x : y : xs) = (y - x) : differences (y : xs)
differences _ = []

onesandthrees :: (Int, Int) -> Int -> (Int, Int)
onesandthrees (o, t) 1 = (o + 1, t)
onesandthrees (o, t) 3 = (o, t + 1)
onesandthrees (o, t) _ = (o, t)

main :: IO ()
main = do
    adaptors <- sort . fmap read . lines <$> getContents
    print $ uncurry (*) $ foldl onesandthrees (0, 1) (differences (0 : adaptors))
