import Data.List
import Data.Maybe

compute :: [Int] -> [(Int, Int)]
compute adaptors = computeInner [(0, 1)] adaptors
    where
        prev n p = fromMaybe 0 $ lookup n p
        computeInner :: [(Int, Int)] -> [Int] -> [(Int, Int)]
        computeInner p [] = p
        computeInner p (a : as) =
            let next = prev (a - 1) p + prev (a - 2) p + prev (a - 3) p in
                computeInner ((a, next) : p) as

main :: IO ()
main = do
    adaptors <- sort . fmap read . lines <$> getContents
    let
        device = maximum adaptors + 3
        levels = compute (adaptors ++ [device])
        in print $ fromJust $ lookup device levels
