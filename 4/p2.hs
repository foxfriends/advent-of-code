import Control.Monad
import Control.Monad.Reader
import Data.List

at x y = reader ((!? x) <=< (!? y))

nextIndex x y = reader (iterateGrid x y)
  where
    iterateGrid x y grid | x + 1 < length (grid !! y) = Just (x + 1, y)
    iterateGrid x y grid | x + 1 == length (grid !! y), y + 1 < length grid = Just (0, y + 1)
    iterateGrid _ _ _ = Nothing

isMas "MAS" = True
isMas "SAM" = True
isMas _ = False

isXmas x y = do
  cross <- readCross
  return
    ( case cross of
        Just cross -> if all isMas cross then 1 else 0
        Nothing -> 0
    )
  where
    readCross = sequence <$> sequence [readWord 1 1, readWord 1 (-1)]
    readWord dx dy = sequence <$> sequence [at (x - dx) (y - dy), at x y, at (x + dx) (y + dy)]

readXmas = readXmas_ 0 0
  where
    readXmas_ x y = do
      count <- isXmas x y
      next <- nextIndex x y
      case next of
        Just (xx, yy) -> (+) count <$> readXmas_ xx yy
        Nothing -> return count

answer = runReader readXmas . lines

main = getContents >>= print . answer
