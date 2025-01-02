import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import Data.Char
import Data.Set qualified as Set

readGrid :: String -> Array (Int, Int) Int
readGrid contents = listArray ((1, 1), (height, width)) $ concat input
  where
    input = fmap (fmap digitToInt) $ lines contents
    width = length $ input !! 0
    height = length input

adjacent (y, x) = [(y + dy, x + dx) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]

thaw_ :: Array (Int, Int) Int -> ST s (STUArray s (Int, Int) Int)
thaw_ = thaw

answer input = runST $ do
  grid <- thaw_ input
  flashes <- newSTRef 0
  forM_ [1..100] $ \_ -> do
    flashed <- consume flashes Set.empty (indices input) grid
    forM_ (indices input) $ \i -> modifyArray' grid i (\v -> if v >= 10 then 0 else v)
  readSTRef flashes
  where
    valid = inRange (bounds input)

    consume count flashed [] _ = return flashed
    consume count flashed (p:ps) grid
      | Set.member p flashed = consume count flashed ps grid
      | otherwise = do
        here <- readArray grid p
        writeArray grid p (here + 1)
        if here + 1 == 10
          then do
            modifySTRef' count (+ 1)
            consume count (Set.insert p flashed) (filter valid (adjacent p) ++ ps) grid
          else consume count flashed ps grid

main = getContents >>= print . answer . readGrid
