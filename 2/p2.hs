absdiff a b = abs (a - b)

isSafe xs = isSafe_ (<) xs || isSafe_ (>) xs || isLessSafe_ (<) (drop 1 xs) || isLessSafe_ (>) (drop 1 xs)
  where
    isSafe_ cmp [] = True
    isSafe_ cmp [_] = True
    isSafe_ cmp (a : b : xs) = (cmp a b && absdiff a b <= 3 && isSafe_ cmp (b : xs)) || isLessSafe_ cmp (a : xs)

    isLessSafe_ cmp [] = True
    isLessSafe_ cmp [_] = True
    isLessSafe_ cmp (a : b : xs) = cmp a b && absdiff a b <= 3 && isLessSafe_ cmp (b : xs)

answer = length . filter isSafe . fmap (fmap read . words) . lines

main = (show . answer) <$> getContents >>= putStrLn
