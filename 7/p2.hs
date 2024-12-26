import Data.List
import Data.Maybe

data Rule = Rule String [(Int, String)] deriving (Show)

mapsnd :: (b -> c) -> (a, b) -> (a, c)
mapsnd f (x, y) = (x, f y)

rule :: String -> Rule
rule str = Rule from to
    where
        a : b : _ : _ : rest = words str
        from = a ++ " " ++ b
        to = bags rest
        bags (n : a : b : _ : rest) = (read n, a ++ " " ++ b) : bags rest
        bags _ = []

ruleFor s (Rule f _) = s == f

mustContain :: String -> [Rule] -> Int
mustContain bag rules =
    let Rule _ to = fromJust $ find (ruleFor bag) rules in
        sum $ fmap (\(x, bag) -> x * (1 + mustContain bag rules)) to

main :: IO ()
main = do
    contents <- getContents
    let rules = fmap rule $ lines contents in
        print $ mustContain "shiny gold" rules
