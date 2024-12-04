import Data.List

both f (a, b) = (f a, f b)

toPair [a, b] = (a, b)

absdiff (a, b) = abs (a - b)

answer = sum . fmap absdiff . uncurry zip . both sort . unzip . fmap (both read . toPair . words) . lines

main = getContents >>= print . answer
