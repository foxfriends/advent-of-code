import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Data.Map (Map)

split :: Eq a => a -> [a] -> [[a]]
split x ys = splitInner x [] ys
    where
        splitInner _ [] [] = []
        splitInner _ r [] = [reverse r]
        splitInner x r (y : ys)
            | x == y = reverse r : splitInner x [] ys
            | otherwise = splitInner x (y : r) ys

run :: Int -> [Int] -> Int
run n st = evalState (run_ n) (Map.fromList $ zip (init st) [1..])
    where
        run_ :: Int -> State (Map Int Int) Int
        run_ n | n <= length st = return $ st !! (n - 1)
        run_ i = do
            prev <- run_ $ i - 1
            before <- gets (Map.lookup prev)
            let next = maybe 0 (i - 1 -) before in do
                modify (Map.insert prev (i - 1))
                return next

main :: IO ()
main = do
    numbers <- fmap read . split ',' <$> getContents
    print $ run 2020 numbers
