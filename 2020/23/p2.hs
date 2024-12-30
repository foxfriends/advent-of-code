import Control.Monad
import Data.Function
import Data.Char
import Data.STRef
import Data.List
import Data.Array.ST
import Data.Array.Unboxed

rotate (x:xs) = xs ++ [x]

wrapdown 1 = 1000000
wrapdown n = n - 1

wrapup 1000000 = 1
wrapup n = n + 1

move :: Int -> [Int] -> UArray Int Int
move start list = runSTUArray $ do
  ring <- newListArray (1, 1000000) list
  current <- newSTRef start
  forM_ [1 .. 10000000] $ \_ -> do
    a <- readSTRef current
    b <- readArray ring a
    c <- readArray ring b
    d <- readArray ring c
    e <- readArray ring d
    writeArray ring a e
    let Just destination = find (`notElem` [b,c,d]) $ tail $ iterate wrapdown a
     in do
      after <- readArray ring destination
      writeArray ring destination b
      writeArray ring d after
      writeSTRef current e
  return ring

ans arr = arr ! 1 * arr ! (arr ! 1)

answer input = ans $ move (head long) ordered
  where
    long = (fmap digitToInt $ init input) ++ [10 .. 1000000]
    ordered = fmap snd $ sortBy (compare `on` fst) $ long `zip` rotate long

main = getContents >>= print . answer
