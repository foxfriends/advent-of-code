import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Text.Parsec

int :: Parsec String s Int
int = read <$> many digit

register ch = do
  string ("Register " ++ ch ++ ": ")
  val <- int
  newline
  return val

parseInput = do
  ra <- register "A"
  rb <- register "B"
  rc <- register "C"
  newline
  string "Program: "
  ops <- int `sepBy` char ','
  return (ra, rb, rc, listArray (0, length ops - 1) ops)

data Registers = Registers {ra :: Int, rb :: Int, rc :: Int, ip :: Int}

combo n | n <= 3 = pure n
combo 4 = gets ra
combo 5 = gets rb
combo 6 = gets rc

apply 0 c = do
  val <- combo c
  modify (\r -> r {ra = ra r `shiftR` val})
apply 1 l = modify (\r -> r {rb = rb r `xor` l})
apply 2 c = do
  val <- combo c
  modify (\r -> r {rb = val .&. 7})
apply 3 l = do
  val <- gets ra
  unless (val == 0) (modify (\r -> r {ip = l}))
apply 4 _ = modify (\r -> r {rb = rb r `xor` rc r})
apply 5 c = do
  val <- combo c
  lift $ tell [val .&. 7]
apply 6 c = do
  val <- combo c
  modify (\r -> r {rb = ra r `shiftR` val})
apply 7 c = do
  val <- combo c
  modify (\r -> r {rc = ra r `shiftR` val})

run :: Array Int Int -> StateT Registers (Writer [Int]) ()
run program = do
  ip <- gets ip
  modify (\r -> r {ip = ip + 2})
  unless (ip > snd (bounds program)) $ do
    apply (program ! ip) (program ! (ip + 1))
    run program

answer :: String -> [Int]
answer contents = execWriter $ evalStateT (run ops) (Registers ra rb rc 0)
  where
    Right (ra, rb, rc, ops) = parse parseInput "" contents

main = getContents >>= putStrLn . intercalate "," . fmap show . answer
