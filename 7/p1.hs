import Data.Bits
import Text.Parsec

int = read <$> many digit

apply _ [ans] = ans
apply n (x : y : rest)
  | testBit n 0 = apply (n `shiftR` 1) (x + y : rest)
  | otherwise = apply (n `shiftR` 1) (x * y : rest)

solvable answer terms = or [answer == apply i terms | i :: Int <- [0 .. 2 ^ (length terms - 1) - 1]]

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
