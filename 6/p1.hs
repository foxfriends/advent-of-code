groups :: [String] -> [[String]]
groups = splitWhere null

splitWhere :: (a -> Bool) -> [a] -> [[a]]
splitWhere = splitWhere_ []
    where
        splitWhere_ [] _ [] = []
        splitWhere_ g _ [] = [g]
        splitWhere_ g f (a : as)
            | f a = reverse g : splitWhere_ [] f as
            | otherwise = splitWhere_ (a : g) f as

uniq :: Eq a => [a] -> [a]
uniq = uniq_ []
    where
        uniq_ as [] = as
        uniq_ u (a : as)
            | a `elem` u = uniq_ u as
            | otherwise = uniq_ (a : u) as

main :: IO ()
main = do
    contents <- getContents
    print (sum $ fmap (length . uniq . concat) (groups $ lines contents))
