import Text.Parsec
import Data.Set qualified as Set
import Data.Set ((\\))
import Data.Map.Strict qualified as Map
import Data.List (foldl1)

food = do
  ingredients <- many1 letter `endBy` char ' '
  string "(contains "
  allergens <- many1 letter `sepBy` string ", "
  char ')'
  return (ingredients, allergens)

answer contents = length $ filter (`elem` safe) $ concat $ (fst <$> foods)
  where
    Right foods = parse (food `endBy` newline) "" contents
    allergens = Set.unions (Set.fromList <$> snd <$> foods)
    ingredients = Set.unions (Set.fromList <$> fst <$> foods)
    candidates = Map.fromList $ Set.toList allergens `zip` fmap (\a -> foldl1 Set.intersection $ fmap (Set.fromList . fst) $ filter (\(_, i) -> a `elem` i) foods) (Set.toList allergens)
    safe = Set.toList $ ingredients \\ (Set.unions $ Map.elems candidates)

main = getContents >>= print . answer
