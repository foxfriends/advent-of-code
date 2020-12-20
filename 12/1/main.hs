data Direction = DN | DE | DS | DW deriving (Show, Eq)

l :: Direction -> Direction
l DN = DW
l DW = DS
l DS = DE
l DE = DN

r :: Direction -> Direction
r DW = DN
r DS = DW
r DE = DS
r DN = DE

data Instruction
    = N Int
    | E Int
    | S Int
    | W Int
    | R Int
    | L Int
    | F Int
    deriving (Show, Eq)

parse :: String -> Instruction
parse ('N' : rest) = N (read rest)
parse ('E' : rest) = E (read rest)
parse ('S' : rest) = S (read rest)
parse ('W' : rest) = W (read rest)
parse ('R' : rest) = R (read rest `div` 90)
parse ('L' : rest) = L (read rest `div` 90)
parse ('F' : rest) = F (read rest)
parse _ = error "Invalid instruction"

run :: [Instruction] -> (Direction, Int, Int) -> (Direction, Int, Int)
run [] = id
run (i : is) = run is . apply i

apply :: Instruction -> (Direction, Int, Int) -> (Direction, Int, Int)
apply (N n) (d, x, y) = (d, x, y + n)
apply (F n) (DN, x, y) = (DN, x, y + n)
apply (S n) (d, x, y) = (d, x, y - n)
apply (F n) (DS, x, y) = (DS, x, y - n)
apply (E n) (d, x, y) = (d, x + n, y)
apply (F n) (DE, x, y) = (DE, x + n, y)
apply (W n) (d, x, y) = (d, x - n, y)
apply (F n) (DW, x, y) = (DW, x - n, y)
apply (L 0) h = h
apply (L n) (d, x, y) = apply (L $ n - 1) (l d, x, y)
apply (R 0) h = h
apply (R n) (d, x, y) = apply (R $ n - 1) (r d, x, y)

main :: IO ()
main = do
    instructions <- fmap parse . lines <$> getContents
    let (_, x, y) = run instructions (DE, 0, 0) in
        print $ abs x + abs y
