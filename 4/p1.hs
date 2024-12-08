import Control.Monad.Reader
import Data.Array.IArray
import Data.Maybe

readGrid :: String -> Array (Int, Int) Char
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ concat input
  where
    input = lines contents
    width = length (input !! 0)
    height = length input

at x y = reader (!? (y, x))

spokesAt x y = sequence [readWord x y | y <- [-1 .. 1], x <- [-1 .. 1], (x, y) /= (0, 0)]
  where
    readWord dx dy = sequence <$> sequence [at (x + i * dx) (y + i * dy) | i <- [0 .. 3]]

answer contents = length $ filter ("XMAS" ==) $ catMaybes $ concat $ runReader (mapM (uncurry spokesAt) $ indices grid) grid
  where
    grid = readGrid contents

main = getContents >>= print . answer
