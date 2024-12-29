import Control.Monad.State
import Data.Graph
import Text.Parsec

int = read <$> many digit

orderingRule = do
  lhs <- int
  char '|'
  rhs <- int
  return (lhs, rhs)

updateSpec = int `sepBy` char ','

isCorrect rules pages = and [(y, pages !! i) `notElem` rules | i <- [0 .. length pages - 1], y <- drop (i + 1) pages]

middle xs = xs !! (length xs `div` 2)

relevant pages (x, y) = x `elem` pages && y `elem` pages

fix rules pages = filter (`elem` pages) $ topSort $ buildG (minimum pages, maximum pages) $ filter (relevant pages) rules

answer input = sum $ middle . fix pageOrder <$> filter (not . isCorrect pageOrder) updates
  where
    (rulestr, "" : updatestr) = break ("" ==) $ lines input
    Right pageOrder = mapM (parse orderingRule "") rulestr
    Right updates = mapM (parse updateSpec "") updatestr

main = getContents >>= print . answer
