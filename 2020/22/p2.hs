import Control.Arrow
import Data.Either
import Data.List
import Data.Set qualified as Set

score = sum . zipWith (*) [1..] . reverse

play a b = play_ Set.empty a b
  where
    play_ _ a [] = Left $ score a
    play_ _ [] b = Right $ score b
    play_ cache a b | Set.member (a, b) cache = Left $ score a
    play_ cache aa@(a:as) bb@(b:bs)
      | a <= length as && b <= length bs = either (const awins) (const bwins) $ play (take a as) (take b bs)
      | a > b = awins
      | b > a = bwins
      where
        cache' = Set.insert (aa, bb) cache
        awins = play_ cache' (as ++ [a, b]) bs
        bwins = play_ cache' as (bs ++ [b, a])

answer contents = either id id $ play p1 p2
  where
    (p1, p2) :: ([Int], [Int]) = (fmap read . tail *** fmap read . drop 2) $ break null $ lines contents

main = getContents >>= print . answer
