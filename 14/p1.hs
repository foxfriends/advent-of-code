import Data.Ix
import Data.Map qualified as Map
import Text.Parsec
import Text.Parsec.Char

width = 101

height = 103

int :: Parsec String m Int
int = do
  neg <- option '+' (char '-')
  n <- read <$> many digit
  return $ if neg == '-' then -n else n

coordinate ch = do
  string (ch : "=")
  x <- int
  char ','
  y <- int
  return (x, y)

robot = do
  pos <- coordinate 'p'
  space
  vel <- coordinate 'v'
  return (pos, vel)

parser = robot `endBy` newline

move n ((x, y), (h, v)) = ((x + (h + width) * n) `rem` width, (y + (v + height) * n) `rem` height)

quadrant pos
  | inRange ((0, 0), (width `div` 2 - 1, height `div` 2 - 1)) pos = 1
  | inRange ((width - width `div` 2, 0), (width, height `div` 2 - 1)) pos = 2
  | inRange ((0, height - height `div` 2), (width `div` 2 - 1, height)) pos = 3
  | inRange ((width - width `div` 2, height - height `div` 2), (width, height)) pos = 4
  | otherwise = 0

answer contents = product $ Map.elems $ Map.delete 0 $ Map.fromListWith (+) ((,1) . quadrant . move 100 <$> robots)
  where
    Right robots = parse parser "" contents

main = getContents >>= print . answer
