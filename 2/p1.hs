import Text.Parsec

int :: Parsec String u Int
int = read <$> many digit

parseInput = do
  low <- int
  char '-'
  high <- int
  char ' '
  ch <- letter
  string ": "
  password <- many letter
  return (low, high, ch, password)

main :: IO ()
main = do
  contents <- getContents
  print $ length [() | line <- lines contents, check line]

check line = low <= len && len <= high
  where
    Right (low, high, char, password) = parse parseInput "" line
    len = length [() | ch <- password, ch == char]
