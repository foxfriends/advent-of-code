import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe

at x y = reader ((!? x) <=< (!? y))

spokesAt x y = sequence [readWord x y | y <- [-1 .. 1], x <- [-1 .. 1], (x, y) /= (0, 0)]
  where
    readWord dx dy = sequence <$> sequence [at (x + i * dx) (y + i * dy) | i <- [0 .. 3]]

answer input = length $ filter ("XMAS" ==) $ catMaybes $ concat $ runReader (mapM (uncurry spokesAt) everywhere) grid
  where
    grid = lines input
    everywhere = [(x, y) | y <- [0 .. length grid - 1], x <- [0 .. length (grid !! y) - 1]]

main = getContents >>= print . answer
