import Data.List
import Data.Map
import Data.Maybe

both f (a, b) = (f a, f b)

toPair [a, b] = (a, b)

count = count_ empty
  where
    count_ acc [] = acc
    count_ acc (x : xs) = count_ (alter (Just . (+ 1) . fromMaybe 0) x acc) xs

answer = sum . uncurry similarity . unzip . fmap (both read . toPair . words) . lines

similarity lhs rhs = fmap score lhs
  where
    counts = count rhs
    score v = v * findWithDefault 0 v counts

main = (show . answer) <$> getContents >>= putStrLn
