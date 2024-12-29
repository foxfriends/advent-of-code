import Data.Array.Unboxed
import Data.Maybe
import Text.Parsec

type Pos = (Int, Int)

type Dir = (Int, Int)

type Grid = Array Pos Char

wall = char '#' >> return "##"

box = char 'O' >> return "[]"

robot = do
  pos <- getPosition
  putState $ Just (sourceLine pos - 1, 2 * (sourceColumn pos - 1))
  char '@'
  return "@."

open = char '.' >> return ".."

tile = wall <|> box <|> open <|> robot

up = char '^' >> return (-1, 0)

right = char '>' >> return (0, 1)

down = char 'v' >> return (1, 0)

left = char '<' >> return (0, -1)

instruction = up <|> down <|> left <|> right

parseInput :: Parsec String (Maybe Pos) (Grid, [Dir], Pos)
parseInput = do
  tiles <- (concat <$> many1 tile) `endBy` newline
  newline
  instructions <- concat <$> many1 instruction `endBy` newline
  Just start <- getState
  return (listArray ((0, 0), (length tiles - 1, length (tiles !! 0) - 1)) $ concat tiles, instructions, start)

add2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

move (grid, pos) dir = fromMaybe (grid, pos) $ do
  pushed <- push pos dir grid
  return (pushed, add2 pos dir)

push pos dir grid =
  case grid ! pos of
    '#' -> Nothing
    '.' -> Just grid
    here | here == '@' || fst dir == 0 -> do
      grid <- push into dir grid
      return $ grid // [(pos, '.'), (into, here)]
    '[' -> do
      grid <- push into dir grid
      grid <- push intobeside dir grid
      return $ grid // [(pos, '.'), (beside, '.'), (into, '['), (intobeside, ']')]
      where
        beside = add2 pos (0, 1)
        intobeside = add2 into (0, 1)
    ']' -> do
      grid <- push into dir grid
      grid <- push intobeside dir grid
      return $ grid // [(pos, '.'), (beside, '.'), (into, ']'), (intobeside, '[')]
      where
        beside = add2 pos (0, -1)
        intobeside = add2 into (0, -1)
  where
    into = add2 pos dir

gps (y, x) = 100 * y + x

answer contents = sum $ fmap (gps . fst) . filter ((== '[') . snd) $ assocs $ fst $ foldl move (grid, start) instructions
  where
    Right (grid, instructions, start) = runParser parseInput Nothing "" contents

main = getContents >>= print . answer
