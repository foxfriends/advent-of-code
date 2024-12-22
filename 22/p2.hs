import Control.Arrow
import Data.Bits
import Data.List
import qualified Data.Map.Strict as Map

prng :: Int -> Int
prng =
  id &&& (* 64)
    >>> uncurry xor
    >>> (`mod` 16777216)
    >>> (id &&& (`div` 32))
    >>> uncurry xor
    >>> (`mod` 16777216)
    >>> (id &&& (* 2048))
    >>> uncurry xor
    >>> (`mod` 16777216)

prices rngs@(a : b : c : d : e : _) = Map.singleton (b - a, c - b, d - c, e - d) e : prices (drop 1 rngs)
prices _ = []

answer = maximum . Map.unionsWith (+) . fmap (Map.unions . prices . fmap (`mod` 10) . take 2000 . iterate prng . read) . lines

main = getContents >>= print . answer
