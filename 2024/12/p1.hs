import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

type Grid = Array Pos Char

readGrid :: String -> Grid
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ concat input
  where
    input = lines contents
    height = length input
    width = length (input !! 0)

visit pos = modify' (Set.insert pos)

been pos = gets (Set.member pos)

neighbours pos = add2 pos <$> [(-1, 0), (1, 0), (0, -1), (0, 1)]

add2 (x, y) (x2, y2) = (x + x2, y + y2)

flood :: Grid -> Pos -> State (Set Pos) (Maybe (Int, Int))
flood grid pos = do
  hasbeen <- been pos
  if hasbeen
    then return Nothing
    else Just <$> flood_ (grid ! pos) pos
  where
    flood_ :: Char -> Pos -> State (Set Pos) (Int, Int)
    flood_ ch pos = do
      hasbeen <- been pos
      if grid !? pos /= Just ch
        then return (0, 1)
        else
          if hasbeen
            then return (0, 0)
            else do
              visit pos
              floods <- mapM (flood_ ch) (neighbours pos)
              return $ foldl add2 (1, 0) floods

answer :: String -> Int
answer contents = sum $ fmap (uncurry (*)) $ catMaybes $ evalState (mapM (flood grid) (indices grid)) Set.empty
  where
    grid = readGrid contents

main = getContents >>= print . answer
