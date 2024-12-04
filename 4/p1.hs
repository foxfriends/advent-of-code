import Control.Monad
import Control.Monad.Reader
import Data.List

at x y = reader ((!? x) <=< (!? y))

nextIndex x y = reader (iterateGrid x y)
  where
    iterateGrid x y grid | x + 1 < length (grid !! y) = Just (x + 1, y)
    iterateGrid x y grid | x + 1 == length (grid !! y), y + 1 < length grid = Just (0, y + 1)
    iterateGrid _ _ _ = Nothing

xmasesFrom x y =
  length . filter (Just "XMAS" ==) <$> sequence spokes
  where
    spokes = [readWord 0 1, readWord 1 0, readWord 0 (-1), readWord (-1) 0, readWord (-1) (-1), readWord (-1) 1, readWord 1 (-1), readWord 1 1]
    readWord dx dy = sequence <$> sequence [at x y, at (x + dx) (y + dy), at (x + 2 * dx) (y + 2 * dy), at (x + 3 * dx) (y + 3 * dy)]

readXmas = readXmas_ 0 0
  where
    readXmas_ x y = do
      count <- xmasesFrom x y
      next <- nextIndex x y
      case next of
        Just (xx, yy) -> (+) count <$> readXmas_ xx yy
        Nothing -> return count

answer = runReader readXmas . lines

main = getContents >>= print . answer
