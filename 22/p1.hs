import Control.Arrow
import Data.Bits

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

answer = sum . fmap ((!! 2000) . iterate prng . read) . lines

main = getContents >>= print . answer
