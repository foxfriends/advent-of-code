import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Array.Unboxed
import Data.Bits
import Data.List
import Numeric
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
  return (ra, rb, rc, ops)

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
  modify (\r -> r {rb = val .&. 0b111})
apply 3 l = do
  val <- gets ra
  unless (val == 0) (modify (\r -> r {ip = l}))
apply 4 _ = modify (\r -> r {rb = rb r `xor` rc r})
apply 5 c = do
  val <- combo c
  lift $ tell [val .&. 0b111]
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

eval ra rb rc ops = (execWriter $ evalStateT (run ops) (Registers ra rb rc 0))

answer contents = findAnswer 0 0
  where
    Right (_, rb, rc, ops) = parse parseInput "" contents
    goal = length ops - 1
    program = listArray (0, length ops - 1) ops
    tryra ra = eval ra rb rc program
    findAnswer n i | n == goal = i `shiftR` 3 -- a little overshot here oh well
    findAnswer n i =
      let out = tryra i
          dig = out !! 0
          exp = ops !! (goal - n - 1)
       in if dig == exp
            then findAnswer (n + 1) (i `shiftL` 3)
            else findAnswer n (i + 1)

main = getContents >>= print . answer
