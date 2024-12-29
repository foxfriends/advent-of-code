{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.List

data Instruction
    = Nop
    | Acc Int
    | Jmp Int

parse :: String -> Instruction
parse (fmap read . stripPrefix "acc +" -> Just i) = Acc i
parse (fmap read . stripPrefix "acc -" -> Just i) = Acc (-i)
parse (fmap read . stripPrefix "jmp +" -> Just i) = Jmp i
parse (fmap read . stripPrefix "jmp -" -> Just i) = Jmp (-i)
parse _ = Nop

exec :: Int -> [Int] -> [Instruction] -> Int
exec i e _ | i `elem` e = 0
exec i e inst = case inst !! i of
    Nop -> exec (i + 1) (i : e) inst
    Acc n -> n + exec (i + 1) (i : e) inst
    Jmp n -> exec (i + n) (i : e) inst

main :: IO ()
main = do
    instructions <- fmap parse . lines <$> getContents
    print $ exec 0 [] instructions
