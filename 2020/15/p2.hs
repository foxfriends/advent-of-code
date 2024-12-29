import Control.Monad.State
import Data.IntMap.Strict (IntMap, (!?))
import Data.IntMap.Strict qualified as IntMap
import Text.Parsec

int :: Parsec String u Int
int = read <$> many digit

parseInput = int `sepBy` char ','

run :: Int -> [Int] -> Int
run target st = run_ (length st + 1) 0 (IntMap.fromList $ zip st [1 ..])
  where
    run_ :: Int -> Int -> IntMap Int -> Int
    run_ i n _ | i == target = n
    run_ i n map =
      let prev = map !? n
          map' = IntMap.insert n i map
       in case prev of
            Nothing -> run_ (i + 1) 0 map'
            Just time -> run_ (i + 1) (i - time) map'

answer contents = run 30000000 input
  where
    Right input = parse parseInput "" contents

main :: IO ()
main = getContents >>= print . answer
