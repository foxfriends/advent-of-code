import Data.Maybe
import Text.Parsec
import Text.Parsec.Char

int :: Parsec String m Int
int = read <$> many digit

puzzle = do
  string "Button A: X+"
  x <- int
  string ", Y+"
  w <- int
  newline
  string "Button B: X+"
  y <- int
  string ", Y+"
  v <- int
  newline
  string "Prize: X="
  z <- int
  string ", Y="
  u <- int
  newline
  return (x, y, 10000000000000 + z, w, v, 10000000000000 + u)

solve (x, y, z, w, v, u) =
  let b = ((w * z - x * u) `div` (y * w - x * v))
      a = (u - b * v) `div` w
   in if a * x + b * y == z && a * w + b * v == u
        then Just (a, b)
        else Nothing

price (a, b) = 3 * a + b

answer contents = sum $ price <$> mapMaybe solve puzzles
  where
    Right puzzles = parse (puzzle `sepBy` newline) "" contents

main = getContents >>= print . answer
