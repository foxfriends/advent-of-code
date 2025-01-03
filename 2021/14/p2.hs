import Control.Arrow
import Text.Parsec
import Data.List
import Data.Map qualified as Map

initialize ps = Map.unionsWith (+) $ fmap (flip Map.singleton 1) $ ps `zip` tail ps

rule = do
  a:b:_ <- count 2 letter
  string " -> "
  to <- letter
  return $ Map.singleton (a, b) [(a, to), (to, b)]

input = do
  initial <- many letter
  count 2 newline
  rules <- rule `endBy` newline
  return (initial, Map.unions rules)

step rules state = Map.unionsWith (+) $ do
  (k, v) <- Map.assocs state
  k2 <- rules Map.! k
  return $ Map.singleton k2 v

answer contents =
  uncurry (-)
    $ maximum &&& minimum
    $ Map.unionWith (+) (Map.singleton (last initial) 1)
    $ Map.mapKeysWith (+) fst
    $ (!! 40)
    $ iterate (step rules)
    $ initialize initial
  where Right (initial, rules) = parse input "" contents

main = getContents >>= print . answer
