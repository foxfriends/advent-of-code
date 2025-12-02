import Text.Parsec

integer = read <$> many digit

range = do
  start :: Int <- integer
  char '-'
  end <- integer
  return [start .. end]

ranges = range `sepBy` (char ',')

isTwice str
  | length str `mod` 2 == 1 = False
  | otherwise = front == back
  where
    front = take (length str `div` 2) str
    back = drop (length str `div` 2) str

answer contents = sum $ read <$> filter isTwice (show <$> concat input)
  where
    Right input = parse ranges "" contents

main = getContents >>= print . answer
