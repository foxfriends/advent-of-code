import Data.Bits
import Text.Parsec

int = read <$> many digit

countdigits n
  | n < 10 = 1
  | otherwise = 1 + countdigits (n `div` 10)

apply [] [ans] = ans
apply (0 : ops) (x : y : rest) = apply ops (x + y : rest)
apply (1 : ops) (x : y : rest) = apply ops (x * y : rest)
apply (2 : ops) (x : y : rest) = apply ops (x * (10 ^ countdigits y) + y : rest)

combinations 0 = [[]]
combinations n = do
  next <- combinations (n - 1)
  op <- [0, 1, 2]
  return (op : next)

solvable answer terms = or [answer == apply ops terms | ops <- combinations (length terms - 1)]

equation = do
  answer <- int
  char ':'
  space
  terms <- int `sepBy` char ' '
  return (answer, terms)

compute = sum . fmap fst . filter (uncurry solvable) <$> equation `sepEndBy` newline

answer input = ans
  where
    Right ans = parse compute "" input

main = getContents >>= print . answer
