import Data.Function
import Data.Ix
import Data.List
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

move ((x, y), (h, v)) =
  (((x + (h + width)) `rem` width, (y + (v + height)) `rem` height), (h, v))

inMiddle robots = length $ filter (inRange ((width `div` 4, height `div` 4), ((width `div` 4) * 3, (height `div` 4) * 3)) . fst) robots

answer contents = fst . maximumBy (compare `on` (inMiddle . snd)) $ take 10000 $ [0 ..] `zip` iterate (fmap move) robots
  where
    Right robots = parse parser "" contents

main = getContents >>= print . answer
