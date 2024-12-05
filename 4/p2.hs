import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe

at x y = reader ((!? x) <=< (!? y))

isMas x = x == "MAS" || x == "SAM"

orr a b c = a c || b c

crossAt x y = sequence <$> sequence [readWord 1 1, readWord 1 (-1)]
  where
    readWord dx dy = sequence <$> sequence [at (x - dx) (y - dy), at x y, at (x + dx) (y + dy)]

answer input = length $ filter (all isMas) $ catMaybes $ runReader (mapM (uncurry crossAt) everywhere) grid
  where
    grid = lines input
    everywhere = [(x, y) | y <- [0 .. length grid - 1], x <- [0 .. length (grid !! y) - 1]]

main = getContents >>= print . answer
