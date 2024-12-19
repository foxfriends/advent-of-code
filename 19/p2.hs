import Data.Either
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Text.Parsec

parseTowels = many letter `sepBy` (string ", ")

memoize p = do
  input <- getInput
  state <- getState
  case state !? input of
    Nothing -> do
      val <- p
      modifyState (Map.insert input val)
      return val
    Just val -> return val

allOf ps = do
  input <- getInput
  pos <- getPosition
  sequence $ fmap (\p -> setInput input >> setPosition pos >> p) ps

parseTarget :: [String] -> Parsec String (Map String Int) Int
parseTarget towels = memoize (sum <$> (allOf $ fmap (\s -> try (string s >> ((eof >> return 1) <|> parseTarget towels)) <|> return 0) towels))

answer input = sum $ rights $ fmap (runParser (parseTarget towels) (Map.empty) "") targets
  where
    towelline : _ : targets = lines input
    Right towels = parse parseTowels "" towelline

main = getContents >>= print . answer
