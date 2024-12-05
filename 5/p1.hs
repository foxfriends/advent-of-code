import Control.Monad.State
import Text.Parsec

int = read <$> many digit

orderingRule = do
  lhs <- int
  char '|'
  rhs <- int
  return (lhs, rhs)

updateRule = int `sepBy` char ','

isCorrect rules pages = and [(y, pages !! i) `notElem` rules | i <- [0 .. length pages - 1], y <- drop (i + 1) pages]

middle xs = xs !! (length xs `div` 2)

answer input = sum $ middle <$> filter (isCorrect pageOrder) updates
  where
    (rulestr, "" : updatestr) = break ("" ==) $ lines input
    Right pageOrder = mapM (parse orderingRule "") rulestr
    Right updates = mapM (parse updateRule "") updatestr

main = getContents >>= print . answer
