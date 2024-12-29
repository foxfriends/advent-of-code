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

check line = (isLeft && not isRight) || (isRight && not isLeft)
  where
    Right (left, right, char, password) = parse parseInput "" line
    isLeft = (password !! (left - 1)) == char
    isRight = (password !! (right - 1)) == char
