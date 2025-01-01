import Text.Parsec

crabs = (read <$> many1 digit) `sepBy` char ','

answer contents = minimum $ fmap fuel [lo..hi]
  where
    Right input = parse crabs "" contents
    lo = minimum input
    hi = maximum input
    fuel n = sum $ fmap (abs . (n -)) input

main = getContents >>= print . answer
