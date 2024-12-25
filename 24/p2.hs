import Data.Bits
import Data.List
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Text.Parsec
import Text.Printf

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

toBits :: Int -> Int -> [Bool]
toBits 0 n = []
toBits i n = (n .&. 1 == 1) : toBits (i - 1) (n `shiftR` 1)

replace ch n = Map.union (Map.fromList $ (\(i, b) -> (ch : printf "%02d" i, C b)) <$> [0 :: Int ..] `zip` toBits 45 n)

mkx :: Int -> Tree
mkx n = CON $ printf "x%02d" n

mky :: Int -> Tree
mky n = CON $ printf "y%02d" n

carry 1 = AND (mkx 0) (mky 0)
carry n = OR (AND (mkx $ n - 1) (mky $ n - 1)) (AND (IG $ carry $ n - 1) (natural $ n - 1))

natural n = XOR (mkx n) (mky n)

expect 0 = natural 0
expect 45 = carry 45
expect n = XOR (carry n) (natural n)

data Tree
  = CON String
  | AND {lt :: Tree, rt :: Tree}
  | OR {lt :: Tree, rt :: Tree}
  | XOR {lt :: Tree, rt :: Tree}
  | IG Tree
  deriving (Show)

data ATree
  = ACON {name :: String}
  | AAND {lhs :: ATree, rhs :: ATree, name :: String}
  | AOR {lhs :: ATree, rhs :: ATree, name :: String}
  | AXOR {lhs :: ATree, rhs :: ATree, name :: String}
  deriving (Show)

findErrors (CON a) (ACON b) | a == b = []
findErrors (IG _) _ = []
findErrors exp@(AND _ _) act@(AAND _ _ _) = sub exp act
findErrors exp@(OR _ _) act@(AOR _ _ _) = sub exp act
findErrors exp@(XOR _ _) act@(AXOR _ _ _) = sub exp act
findErrors _ act = [name act]

sub exp act = if null le || null re then le ++ re else [name act]
  where
    ll = findErrors (lt exp) (lhs act)
    rl = findErrors (rt exp) (lhs act)
    lr = findErrors (lt exp) (rhs act)
    rr = findErrors (rt exp) (rhs act)
    le = if null rl || null ll then [] else rl ++ ll
    re = if null lr || null rr then [] else lr ++ rr

answer contents = fmap (uncurry findErrors) $ fmap expect [0 ..] `zip` [subcircuit n | n <- Map.keys circuit, head n == 'z']
  where
    Right circuit = parse wires "" contents
    subcircuit from =
      case circuit ! from of
        C v -> ACON from
        A l r -> AAND (subcircuit l) (subcircuit r) from
        O l r -> AOR (subcircuit l) (subcircuit r) from
        X l r -> AXOR (subcircuit l) (subcircuit r) from

main = getContents >>= print . answer

-- djg,dsd,hjm,mcq,sbg,z12,z19,z37
