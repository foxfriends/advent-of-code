{-# LANGUAGE ViewPatterns #-}

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

type Dir = (Int, Int)

type Grid = Array Pos Char

readGrid :: String -> Grid
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ concat input
  where
    input = lines contents
    height = length input
    width = length (input !! 0)

visit pos = modify' (Set.insert pos)

been pos = gets (Set.member pos)

add2 (a1, p1) (a2, p2) = (a1 + a2, p1 + p2)

collect (a1, p1) (a2, p2) = (a1 + a2, Set.union p1 p2)

neighbours pos = fmap (add2 pos) directions

directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

turn (y, x) = (x, -y)

unturn = turn . turn . turn

fence pos = add2 pos &&& id

flood :: Grid -> Pos -> State (Set Pos) (Maybe (Int, Set (Pos, Dir)))
flood grid pos = do
  hasbeen <- been pos
  if hasbeen
    then return Nothing
    else Just <$> flood_ (grid ! pos) pos
  where
    flood_ :: Char -> Pos -> State (Set Pos) (Int, Set (Pos, Dir))
    flood_ ch pos = do
      hasbeen <- been pos
      if grid !? pos /= Just ch || hasbeen
        then return (0, Set.empty)
        else do
          visit pos
          floods <- mapM (flood_ ch) (neighbours pos)
          let fenceSides = filter (\p -> grid !? add2 pos p /= Just ch) directions
           in return $ foldl collect (1, Set.fromList (fence pos <$> fenceSides)) floods

sides :: Set (Pos, Dir) -> Int
sides (Set.minView -> Nothing) = 0
sides (Set.minView -> Just (seg, rest)) = 1 + sides (go unturn seg $ go turn seg rest)
  where
    go rot (pos, dir) rest
      | Set.member seg rest = go rot seg $ Set.delete seg rest
      | otherwise = rest
      where
        target = add2 pos $ rot dir
        seg = (target, dir)

answer :: String -> Int
answer contents = sum $ fmap (uncurry (*) . second sides) $ catMaybes $ evalState (mapM (flood grid) (indices grid)) Set.empty
  where
    grid = readGrid contents

main = getContents >>= print . answer
