{-# LANGUAGE ViewPatterns #-}
import Data.List
import Data.Bits

split :: Eq a => [a] -> [a] -> [[a]]
split x ys = splitInner x [] ys
    where
        splitInner _ [] [] = []
        splitInner _ r [] = [reverse r]
        splitInner x r (stripPrefix x -> Just ys) = reverse r : splitInner x [] ys
        splitInner x r (y : ys) = splitInner x (y : r) ys

data Instruction
    = Mask [Maybe Int]
    | Set Int Int

parsebit :: Char -> Maybe Int
parsebit '1' = Just 1
parsebit '0' = Just 0
parsebit _ = Nothing

parse :: String -> Instruction
parse (stripPrefix "mask = " -> Just mask) = Mask (parsebit <$> mask)
parse (stripPrefix "mem[" -> Just rest) = Set loc val
    where [loc, val] = read <$> split "] = " rest

set :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
set loc val map = case findIndex ((==) loc . fst) map of
    Nothing -> (loc, val) : map
    Just n -> (loc, val) : take n map ++ drop (n + 1) map

apply :: [Maybe Int] -> Int -> Int
apply mask val = foldl' applyBit val ([0..] `zip` reverse mask)
    where applyBit :: Int -> (Int, Maybe Int) -> Int
          applyBit val (i, Nothing) = val
          applyBit val (i, Just 0) = val .&. complement (bit i)
          applyBit val (i, Just 1) = val .|. bit i

run :: [Instruction] -> [(Int, Int)]
run = run_ [] (replicate 36 Nothing)
    where
        run_ mem _ [] = mem
        run_ mem _ (Mask mask : is) = run_ mem mask is
        run_ mem mask (Set loc val : is) = run_ (set loc (apply mask val) mem) mask is

main :: IO ()
main = do
    instructions <- fmap parse . lines <$> getContents
    print $ sum $ snd <$> run instructions
