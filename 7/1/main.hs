import Control.Monad
import Data.List
import Data.Maybe

data Rule = Rule String [String] deriving (Show)

mapsnd :: (b -> c) -> (a, b) -> (a, c)
mapsnd f (x, y) = (x, f y)

rule :: String -> Rule
rule str = Rule from to
    where
        a : b : _ : _ : rest = words str
        from = a ++ " " ++ b
        to = bags rest
        bags (_ : a : b : _ : rest) = (a ++ " " ++ b) : bags rest
        bags _ = []

ruleFor s (Rule f _) = s == f

directlyAllowed :: Rule -> (String, Maybe Bool)
directlyAllowed (Rule k []) = (k, Just False)
directlyAllowed (Rule k rs)
    | "shiny gold" `elem` rs = (k, Just True)
    | otherwise = (k, Nothing)

check :: [Rule] -> [(String, Maybe Bool)] -> [(String, Bool)]
check rules pairs
    | all (isJust . snd) pairs = fmap (mapsnd fromJust) pairs
    | otherwise = check rules $ fmap allowed pairs
    where
        allowed (s, Just n) = (s, Just n)
        allowed (s, Nothing) = (s, updated)
            where
                Rule _ to = fromJust (find (ruleFor s) rules)
                children = fmap (\s -> snd . fromJust $ find ((== s) . fst) pairs) to
                updated = foldM (\b c -> (|| b) <$> c) False children :: Maybe Bool

main :: IO ()
main = do
    contents <- getContents
    let
        rules = fmap rule $ lines contents
        allowed = fmap directlyAllowed rules
        in print $ length $ filter snd $ check rules allowed
