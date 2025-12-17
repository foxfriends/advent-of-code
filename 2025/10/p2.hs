import Control.Monad (guard)
import Control.Monad.State
import Data.Bits
import Data.Bool
import Data.Function ((&))
import Data.List (find, foldl1, nub, sort, sortBy, sum)
import Data.Map (Map, insert)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Text.Parsec (between, char, digit, many, many1, parse, sepBy1, sepEndBy, (<|>))

integer = read <$> many digit

machine = do
  _ :: [Char] <- between (char '[') (char ']') $ many1 (char '.' <|> char '#')
  char ' '
  buttons :: [[Int]] <- (between (char '(') (char ')') $ integer `sepBy1` char ',') `sepEndBy` char ' '
  joltage :: [Int] <- between (char '{') (char '}') $ integer `sepBy1` char ','
  return $ (joltage, buttons)

targetState '.' = False
targetState '#' = True

buttonBits len button = foldl setBit 0 $ map (\n -> len - n - 1) button

decrAt 0 (m : ms) = m - 1 : ms
decrAt i (m : ms) = m : decrAt (i - 1) ms

applyButton target [] = target
applyButton target (i : rest) = applyButton (decrAt i target) rest

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n [] = []
choose n (x : xs) = ((x :) <$> choose (n - 1) xs) ++ choose n xs

solveBits :: [[Int]] -> Int -> Int -> [[[Int]]]
solveBits buttons len target =
  [option | n <- [0 .. length buttons], option <- choose n buttons, target == (foldl xor 0 $ fmap (buttonBits len) option)]

solve buttons joltage = evalState (solve_ joltage) Map.empty
  where
    solve_ :: [Int] -> State (Map [Int] Int) Int
    solve_ joltage = do
      cached <- gets (Map.lookup joltage)
      case cached of
        Nothing -> do
          ans <- compute joltage
          modify $ insert joltage ans
          return ans
        Just answer -> return answer
    compute :: [Int] -> State (Map [Int] Int) Int
    compute joltage =
      if all (== 0) joltage
        then return 0
        else
          joltage
            & fmap (\n -> n `mod` 2)
            & foldl (\joltage bit -> bit .|. joltage `shift` 1) 0
            & solveBits buttons (length joltage)
            & fmap (\sol -> (foldl applyButton joltage sol, length sol))
            & filter (\(remaining, _) -> all (>= 0) remaining)
            & mapM
              ( \(remaining, score) -> do
                  sub <- solve_ (fmap (`div` 2) remaining)
                  return $ sub * 2 + score
              )
            & fmap (foldl min 10000000)

answer input = sum $ fmap (\(j, b) -> solve b j) machines
  where
    Right machines = parse (machine `sepEndBy` char '\n') "" input

main = getContents >>= print . answer
