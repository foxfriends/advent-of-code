{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe

data Instruction
    = Nop Int
    | Acc Int
    | Jmp Int

swap :: Instruction -> Maybe Instruction
swap (Nop i) = Just (Jmp i)
swap (Jmp i) = Just (Nop i)
swap _ = Nothing

parse :: String -> Instruction
parse (fmap read . stripPrefix "acc +" -> Just i) = Acc i
parse (fmap read . stripPrefix "acc -" -> Just i) = Acc (-i)
parse (fmap read . stripPrefix "jmp +" -> Just i) = Jmp i
parse (fmap read . stripPrefix "jmp -" -> Just i) = Jmp (-i)
parse (fmap read . stripPrefix "nop +" -> Just i) = Nop i
parse (fmap read . stripPrefix "nop -" -> Just i) = Nop (-i)
parse _ = error "invalid"

exec :: Int -> [Int] -> [Instruction] -> Maybe Int
exec i e _ | i `elem` e = Nothing
exec i _ inst | i >= length inst = Just 0
exec i e inst = case inst !! i of
    Nop _ -> exec (i + 1) (i : e) inst
    Acc n -> (n +) <$> exec (i + 1) (i : e) inst
    Jmp n -> exec (i + n) (i : e) inst

connect :: [Instruction] -> [Instruction] -> Instruction -> [Instruction]
connect ls rs i = ls ++ (i : rs)

fix :: [Instruction] -> Int
fix inst = fromJust $ join $ find isJust $ zipWith3 try (inits (init inst)) (tails (tail inst)) (swap <$> inst)
    where
        try :: [Instruction] -> [Instruction] -> Maybe Instruction -> Maybe Int
        try ls rs x = x <&> connect ls rs >>= exec 0 []

main :: IO ()
main = do
    instructions <- fmap parse . lines <$> getContents
    print $ fix instructions
