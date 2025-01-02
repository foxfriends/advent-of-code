check _ [] = 0
check stack ('(':line) = check ('(':stack) line
check stack ('[':line) = check ('[':stack) line
check stack ('{':line) = check ('{':stack) line
check stack ('<':line) = check ('<':stack) line
check ('(':stack) (')':line) = check stack line
check ('[':stack) (']':line) = check stack line
check ('{':stack) ('}':line) = check stack line
check ('<':stack) ('>':line) = check stack line
check _ (')':line) = 3
check _ (']':line) = 57
check _ ('}':line) = 1197
check _ ('>':line) = 25137

main = getContents >>= print . sum . fmap (check []) . lines
