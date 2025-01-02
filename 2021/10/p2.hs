import Data.List (sort)

check stack [] = stack
check stack ('(':line) = check ('(':stack) line
check stack ('[':line) = check ('[':stack) line
check stack ('{':line) = check ('{':stack) line
check stack ('<':line) = check ('<':stack) line
check ('(':stack) (')':line) = check stack line
check ('[':stack) (']':line) = check stack line
check ('{':stack) ('}':line) = check stack line
check ('<':stack) ('>':line) = check stack line
check stack (')':line) = []
check stack (']':line) = []
check stack ('}':line) = []
check stack ('>':line) = []

score a '(' = 5 * a + 1
score a '[' = 5 * a + 2
score a '{' = 5 * a + 3
score a '<' = 5 * a + 4

middle ls = ls !! (length ls `div` 2)

main = getContents >>= print . middle . dropWhile (== 0) . sort . fmap (foldl score 0 . check []) . lines
