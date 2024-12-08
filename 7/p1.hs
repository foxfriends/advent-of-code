import Control.Monad
import Text.Parsec

int = read <$> many digit

countdigits n
  | n < 10 = 1
  | otherwise = 1 + countdigits (n `div` 10)

apply :: (Int -> Int -> Int) -> [Int] -> [Int]
apply op (x : y : rest) = op x y : rest

findAnswer ans [a]
  | a == ans = [()]
  | otherwise = []
findAnswer ans terms = do
  apd <- apply <$> [(+), (*)] <*> [terms]
  findAnswer ans apd

solvable = not . null . uncurry findAnswer

equation = do
  answer <- int
  char ':'
  space
  terms <- int `sepBy` char ' '
  return (answer, terms)

compute = sum . fmap fst . filter solvable <$> equation `sepEndBy` newline

answer input = ans
  where
    Right ans = parse compute "" input

main = getContents >>= print . answer
