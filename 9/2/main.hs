import Data.Maybe

findInvalid :: [Int] -> [Int] -> Int
findInvalid _ [] = error "invalid"
findInvalid preamble (x : xs) = if x `elem` valid preamble then findInvalid (x : init preamble) xs else x

valid :: [Int] -> [Int]
valid preamble = [(preamble !! i) + (preamble !! j) | i <- [0..23], j <- [i+1..24]]

rangeOf :: Int -> [Int] -> [Int]
rangeOf x xs = fromMaybe (rangeOf x (tail xs)) $ findRange x xs 0
    where
        findRange :: Int -> [Int] -> Int -> Maybe [Int]
        findRange t (x : xs) a
            | x + a == t = Just [x]
            | x + a < t = (x :) <$> findRange t xs (x + a)
            | otherwise = Nothing

main :: IO ()
main = do
    numbers <- fmap read . lines <$> getContents :: IO [Int]
    let (preamble, contents) = splitAt 25 numbers
        invalid = findInvalid (reverse preamble) contents
        range = rangeOf invalid numbers
        in print $ minimum range + maximum range
