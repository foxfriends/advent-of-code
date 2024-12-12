import Control.Monad
import Control.Monad.State
import Data.Array.Unboxed
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set

mapsnd f (x, y) = (x, f y)

mapfst f (x, y) = (f x, y)

both f (x, y) = (f x, f y)

type Pos = (Int, Int)

type Dir = (Int, Int)

type Grid = Array Pos Char

readGrid :: String -> Grid
readGrid contents = listArray ((0, 0), (height - 1, width - 1)) $ concat input
  where
    input = lines contents
    height = length input
    width = length (input !! 0)

visit pos = modify' (mapsnd $ Set.insert pos)

been pos = gets (Set.member pos . snd)

at (y, x) = gets ((!? (y, x)) . fst)

add2 (a1, p1) (a2, p2) = (a1 + a2, p1 + p2)

neighbours pos = fmap (add2 pos) directions

directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

turn (-1, 0) = (0, 1)
turn (0, 1) = (1, 0)
turn (1, 0) = (0, -1)
turn (0, -1) = (-1, 0)

unturn = turn . turn . turn

flood :: Pos -> State (Grid, Set Pos) (Maybe (Int, Set (Pos, Dir)))
flood pos = do
  hasbeen <- been pos
  if hasbeen
    then return Nothing
    else do
      ch <- at pos
      case ch of
        Nothing -> return Nothing
        Just ch -> Just <$> flood_ ch pos
  where
    flood_ :: Char -> Pos -> State (Grid, Set Pos) (Int, Set (Pos, Dir))
    flood_ ch pos = do
      here <- at pos
      hasbeen <- been pos
      if here /= Just ch || hasbeen
        then return (0, Set.empty)
        else do
          visit pos
          nchars <- filterM (pure . (/= Just ch) <=< at . add2 pos) directions
          floods <- mapM (flood_ ch) (neighbours pos)
          return $ foldl (\(a1, p1) (a2, p2) -> (a1 + a2, Set.union p1 p2)) (1, Set.fromList (fmap (\d -> (add2 pos d, d)) nchars)) floods

sides :: Set (Pos, Dir) -> Int
sides walls
  | Set.null walls = 0
  | otherwise =
      let Just (seg, rest) = Set.minView walls
       in 1 + (sides $ go unturn seg $ go turn seg rest)
  where
    go rot (pos, dir) rest =
      let target = add2 pos $ rot dir
       in if Set.member (target, dir) rest
            then go rot (target, dir) (Set.delete (target, dir) rest)
            else rest

answer :: String -> Int
answer contents = sum $ fmap (uncurry (*)) $ fmap (mapsnd sides) $ catMaybes $ evalState (mapM flood (indices grid)) (grid, Set.empty)
  where
    grid = readGrid contents

main = getContents >>= print . answer
