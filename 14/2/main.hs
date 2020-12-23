{-# LANGUAGE ViewPatterns #-}
import Data.Map (Map, insert, empty, elems)
import Data.List (stripPrefix, foldl', replicate)
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

setAll :: [Int] -> Int -> Map Int Int -> Map Int Int
setAll locs val map = foldl' (flip (`insert` val)) map locs

apply :: [Maybe Int] -> Int -> [Int]
apply mask = applyBits 0 (reverse mask)
    where applyBits :: Int -> [Maybe Int] -> Int -> [Int]
          applyBits _ [] x = [x]
          applyBits i (Just 0 : ms) x = applyBits (i + 1) ms x
          applyBits i (Just 1 : ms) x = flip setBit i <$> applyBits (i + 1) ms x
          applyBits i (Nothing : ms) x = [flip setBit i, flip clearBit i] <*> applyBits (i + 1) ms x

run :: [Instruction] -> Map Int Int
run = run_ empty (replicate 36 Nothing)
    where
        run_ mem _ [] = mem
        run_ mem _ (Mask mask : is) = run_ mem mask is
        run_ mem mask (Set loc val : is) = run_ (setAll (apply mask loc) val mem) mask is

main :: IO ()
main = do
    instructions <- fmap parse . lines <$> getContents
    print $ sum $ elems $ run instructions
