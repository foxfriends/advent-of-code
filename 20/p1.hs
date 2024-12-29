import Text.Parsec

makeTile grid = []

parseTile = do
  string "Tile "
  id :: Int <- read <$> many digit
  char ':'
  newline
  rows <- count 10 (oneOf ".#") `endBy` newline
  return (id, makeTile rows)

parseTiles = parseTile `endBy` newline

answer contents = 0
  where
    Right input = parse parseTiles "" contents

main = getContents >>= print . answer
