import Control.Monad
import Data.Ix
import Data.Set qualified as Set
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

answer contents = iterate (fmap move) robots
  where
    Right robots = parse parser "" contents

render scene = unlines $ do
  y <- range (0, height - 1)
  sequence $ do
    x <- range (0, width - 1)
    return $ if (x, y) `Set.member` set then ['*'] else ['.']
  where
    set = Set.fromList (fmap fst scene)

animate n scene = do
  writeFile ("renders/" ++ show n) $ render scene
  when (n < 10000) $ animate (n + 1) $ fmap move scene

main = do
  Right contents <- parse parser "" <$> getContents
  animate 0 contents
