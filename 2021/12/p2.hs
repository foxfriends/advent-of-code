import Text.Parsec
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Map ((!))
import Data.Char

parseEdge = do
  l <- many letter
  char '-'
  r <- many letter
  return $ Map.fromList [(r, [l]), (l, [r])]

parseGraph = Map.unionsWith (++) <$> parseEdge `endBy` newline

allPaths graph = paths False Set.empty "start"
  where
    paths dup visited here
      | here == "end" = 1
      | all isLower here && Set.member here visited && here == "start" = 0
      | all isLower here && Set.member here visited && dup = 0
      | all isLower here && Set.member here visited = continue True inserted here
      | all isLower here = continue dup inserted here
      | otherwise = continue dup visited here
      where
        inserted = Set.insert here visited
        continue dup visited here = sum $ do
          next <- graph ! here
          return $ paths dup visited next

answer contents = allPaths graph
  where
    Right graph = parse parseGraph "" contents

main = getContents >>= print . answer
