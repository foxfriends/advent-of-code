import Control.Arrow
import Data.List (sortBy)
import Data.Function
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Set ((\\))

(∩) = Set.intersection
(∪) = Set.union

decoder :: [Set.Set Char] -> Map.Map (Set.Set Char) Int
decoder [one, seven, four, p1, p2, p3, h1, h2, h3, eight]
  = Map.fromList $
    [ (zero, 0)
    , (one, 1)
    , (two, 2)
    , (three, 3)
    , (four, 4)
    , (five, 5)
    , (six, 6)
    , (seven, 7)
    , (eight, 8)
    , (nine, 9)
    ]
  where
    zero = eight \\ (four ∩ five ∩ two)
    two = (p1 ∩ p2 ∩ p3) ∪ (eight \\ five) ∪ (eight \\ four)
    three = (p1 ∩ p2 ∩ p3) ∪ one
    five = (h1 ∩ h2 ∩ h3) ∪ (p1 ∩ p2 ∩ p3)
    six = eight \\ (two ∩ one)
    nine = eight \\ (eight \\ three \\ four)

decode :: Map.Map (Set.Set Char) Int -> [Set.Set Char] -> Int
decode map = foldl1 (\a b -> 10 * a + b) . fmap (map Map.!)

answer = uncurry decode . (makeDecoder &&& makeInput) . words
  where
    makeDecoder = decoder . sortBy (compare `on` Set.size) . fmap Set.fromList . take 10
    makeInput = fmap Set.fromList . drop 11

main = getContents >>= print . sum . fmap answer . lines
