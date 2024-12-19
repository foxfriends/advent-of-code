import Data.Either
import Text.Parsec

parseTowels = many letter `sepBy` string ", "

parseTarget towels = choice $ fmap (\s -> try (string s >> (eof <|> parseTarget towels))) towels

answer input = length $ rights $ parse (parseTarget towels) "" <$> targets
  where
    towelline : _ : targets = lines input
    Right towels = parse parseTowels "" towelline

main = getContents >>= print . answer
