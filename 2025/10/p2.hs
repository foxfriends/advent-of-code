import Debug.Trace
import Text.Parsec
import Data.List (sortBy, sort, foldl1, find, nub, sum)
import Control.Monad (guard)
import Data.Ord (comparing)

integer = read <$> many digit

machine = do
  _ :: [Char] <- between (char '[') (char ']') $ many1 (char '.' <|> char '#')
  char ' '
  buttons :: [[Int]] <- (between (char '(') (char ')') $ integer `sepBy1` char ',') `sepEndBy` char ' '
  joltage :: [Int] <- between (char '{') (char '}') $ integer `sepBy1` char ','
  return $ (joltage, buttons)

limitWithin target button = foldl1 min $ fmap (target !!) button

subAt 0 n (m:ms) = m - n:ms
subAt i n (m:ms) = m:subAt (i - 1) n ms

applyButton _ [] target = target
applyButton n (i:rest) target = applyButton n rest $ subAt i n $ target

covers target coverage =
  all (\ i -> target !! i == 0 || i `elem` coverage) [0..length target - 1]

solvable _ _ [] = False
solvable remaining target allButtons@(button:buttons) =
  if remaining < foldl1 max target || not (covers target $ nub $ concat allButtons)
    then False
  else
    let limit = min remaining $ limitWithin target button in any pressAndSolve [limit, limit-1..0]
    where
      pressAndSolve times =
        let newTarget = applyButton times button target in
        if all ((==) 0) newTarget
          then True
          else solvable (remaining - times) newTarget (prioritize newTarget buttons)

rangeSize target buttons button =
  let
    others = nub $ concat $ filter ((/=) button) buttons
    upper = limitWithin target button
    in if covers target others then 1 else upper

prioritize target buttons =
  sortBy (comparing $ rangeSize target buttons) buttons

solve (target, buttons) =  ans
  where
    solveIn i = solvable (traceShowId i) target  $ prioritize target buttons
    Just ans = find solveIn [foldl1 max target..]

answer input = sum $ (traceShowId . solve . traceShowId) <$> machines
  where
    Right machines = parse (machine `sepEndBy` char '\n') "" input
main = getContents >>= print . answer
