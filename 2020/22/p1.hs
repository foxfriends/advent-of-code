import Control.Arrow
import Data.List

score = sum . zipWith (*) [1..] . reverse

play a [] = score a
play [] b = score b
play (a:as) (b:bs)
  | a > b = play (as ++ [a, b]) bs
  | b > a = play as (bs ++ [b, a])

answer contents = play p1 p2
  where
    (p1, p2) :: ([Int], [Int]) = (fmap read . tail *** fmap read . drop 2) $ break null $ lines contents

main = getContents >>= print . answer
