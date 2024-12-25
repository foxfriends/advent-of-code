import Data.Bits
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Text.Parsec

data Wire = C Bool | A String String | O String String | X String String

constantWire = do
  name <- many alphaNum
  string ": "
  val <- oneOf "10"
  return (name, C (val == '1'))

binaryWire = do
  lhs <- many alphaNum
  space
  op <- (string "AND" >> return A) <|> (string "OR" >> return O) <|> (string "XOR" >> return X)
  space
  rhs <- many alphaNum
  string " -> "
  name <- many alphaNum
  return (name, op lhs rhs)

wires =
  Map.fromList <$> do
    constants <- constantWire `endBy` newline
    newline
    circuits <- binaryWire `endBy` newline
    return (constants ++ circuits)

eval circuit = memoized
  where
    memoized i = eval_ (circuit ! i)
    eval_ (C b) = b
    eval_ (A lhs rhs) = memoized lhs && memoized rhs
    eval_ (O lhs rhs) = memoized lhs || memoized rhs
    eval_ (X lhs rhs) = memoized lhs /= memoized rhs

toInt :: [Bool] -> Int
toInt [] = 0
toInt (True : bits) = (toInt bits `shiftL` 1) .|. 1
toInt (False : bits) = toInt bits `shiftL` 1

answer contents = toInt [get n | n <- Map.keys circuit, head n == 'z']
  where
    Right circuit = parse wires "" contents
    get = eval circuit

main = getContents >>= print . answer
