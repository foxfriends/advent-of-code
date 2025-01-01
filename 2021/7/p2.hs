import Text.Parsec

crabs = (read <$> many1 digit) `sepBy` char ','

total n = (n * (n + 1)) `div` 2

answer contents = minimum $ fmap fuel [lo..hi]
  where
    Right input = parse crabs "" contents
    lo = minimum input
    hi = maximum input
    fuel n = sum $ fmap (total . abs . (n -)) input

main = getContents >>= print . answer
