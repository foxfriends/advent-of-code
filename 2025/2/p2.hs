import Text.Parsec

integer = read <$> many digit

range = do
  start :: Int <- integer
  char '-'
  end <- integer
  return [start .. end]

ranges = range `sepBy` (char ',')

takeEvery _ "" = []
takeEvery n str = take n str : takeEvery n (drop n str)

isRepeat str = any isRepeat_ [1 .. (length str `div` 2)]
  where
    isRepeat_ n
      | length str `mod` n /= 0 = False
      | otherwise = all (== (take n str)) (takeEvery n str)

answer contents = sum $ read <$> filter isRepeat (show <$> concat input)
  where
    Right input = parse ranges "" contents

main = getContents >>= print . answer
