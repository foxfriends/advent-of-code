import Text.Parsec
import Data.Bifunctor
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Map.Strict qualified as Map
import Data.Map (Map)
import Data.List

food = do
  ingredients <- many1 letter `endBy` char ' '
  string "(contains "
  allergens <- many1 letter `sepBy` string ", "
  char ')'
  return (ingredients, allergens)

solve :: [(String, Set String)] -> Map String String
solve [] = Map.empty
solve candidates = Map.fromList singles `Map.union` rest
  where
    singles = second Set.findMin <$> filter (\(_, c) -> Set.size c == 1) candidates
    eliminating = snd <$> singles
    eliminate = Set.filter (`notElem` eliminating)
    rest = solve $ filter (not . null . snd) $ fmap (second eliminate) candidates

answer contents = intercalate "," $ Map.elems $ solve candidates
  where
    Right foods = parse (food `endBy` newline) "" contents
    allergens = Set.unions (Set.fromList <$> snd <$> foods)
    ingredients = Set.unions (Set.fromList <$> fst <$> foods)
    candidates = Set.toList allergens `zip` fmap (\a -> foldl1 Set.intersection $ fmap (Set.fromList . fst) $ filter (\(_, i) -> a `elem` i) foods) (Set.toList allergens)

main = getContents >>= putStrLn . answer
