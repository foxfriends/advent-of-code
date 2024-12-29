import Control.Arrow

npos '7' = (0, 0)
npos '8' = (0, 1)
npos '9' = (0, 2)
npos '4' = (1, 0)
npos '5' = (1, 1)
npos '6' = (1, 2)
npos '1' = (2, 0)
npos '2' = (2, 1)
npos '3' = (2, 2)
npos '0' = (3, 1)
npos 'A' = (3, 2)

dpos '^' = (0, 1)
dpos 'A' = (0, 2)
dpos '<' = (1, 0)
dpos 'v' = (1, 1)
dpos '>' = (1, 2)

ysym dy
  | dy < 0 = '^'
  | otherwise = 'v'

xsym dx
  | dx < 0 = '<'
  | otherwise = '>'

keypad mapping ban seq = fmap mapping ('A' : seq) `zip` fmap mapping seq >>= uncurry path
  where
    path (y1, x1) (y2, x2) =
      if ban == (y1, x2) || (x2 > x1 && ban /= (y2, x1)) then yseq ++ xseq ++ "A" else xseq ++ yseq ++ "A"
      where
        yseq = uncurry replicate $ (abs &&& ysym) (y2 - y1)
        xseq = uncurry replicate $ (abs &&& xsym) (x2 - x1)

numeric = keypad npos (3, 0)

directional = keypad dpos (0, 0)

answer = sum . fmap (uncurry (*) . (read . take 3 &&& length . directional . directional . numeric)) . lines

main = getContents >>= print . answer
