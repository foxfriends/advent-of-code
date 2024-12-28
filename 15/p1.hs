import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Parsec

int :: Parsec String u Int
int = read <$> many digit

parseInput = int `sepBy` char ','

run :: Int -> [Int] -> Int
run n st = evalState (run_ n) (Map.fromList $ zip (init st) [1 ..])
  where
    run_ :: Int -> Control.Monad.State.State (Map Int Int) Int
    run_ n | n <= length st = return $ st !! (n - 1)
    run_ i = do
      prev <- run_ $ i - 1
      before <- gets (Map.lookup prev)
      let next = maybe 0 (i - 1 -) before
       in do
            modify (Map.insert prev (i - 1))
            return next

answer contents = run 2020 input
  where
    Right input = parse parseInput "" contents

main :: IO ()
main = getContents >>= print . answer
