import Control.Monad.Reader
import Data.Array.IArray
import Data.Maybe

readGrid :: String -> Array (Int, Int) Char
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ concat input
  where
    input = lines contents
    width = length (input !! 0)
    height = length input

at x y = reader (flip (!?) (y, x))

isMas x = x == "MAS" || x == "SAM"

crossAt x y = sequence <$> sequence [readWord 1 1, readWord 1 (-1)]
  where
    readWord dx dy = sequence <$> sequence [at (x - dx) (y - dy), at x y, at (x + dx) (y + dy)]

answer contents = length $ filter (all isMas) $ catMaybes $ runReader (mapM (uncurry crossAt) $ indices grid) grid
  where
    grid = readGrid contents

main = getContents >>= print . answer
