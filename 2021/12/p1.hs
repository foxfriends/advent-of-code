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

allPaths graph = paths Set.empty "start"
  where
    paths visited here
      | here == "end" = 1
      | all isLower here && Set.member here visited = 0
      | all isLower here = continue (Set.insert here visited) here
      | otherwise = continue visited here
      where
        continue visited here = sum $ do
          next <- graph ! here
          return $ paths visited next

answer contents = allPaths graph
  where
    Right graph = parse parseGraph "" contents

main = getContents >>= print . answer
