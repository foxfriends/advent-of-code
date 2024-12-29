absdiff a b = abs (a - b)

isSafe xs = isSafe_ (<) xs || isSafe_ (>) xs
  where
    isSafe_ cmp [] = True
    isSafe_ cmp [_] = True
    isSafe_ cmp (a : b : xs) = cmp a b && absdiff a b <= 3 && isSafe_ cmp (b : xs)

omissions [] = []
omissions (x : xs) = xs : ((x :) <$> omissions xs)

answer = length . filter (any isSafe . omissions) . fmap (fmap read . words) . lines

main = getContents >>= print . answer
