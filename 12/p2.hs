data Instruction
    = N Int
    | E Int
    | S Int
    | W Int
    | R Int
    | L Int
    | F Int
    deriving (Show, Eq)

l :: Pos -> Pos
l (x, y) = (-y, x)

r :: Pos -> Pos
r (x, y) = (y, -x)

parse :: String -> Instruction
parse ('N' : rest) = N (read rest)
parse ('E' : rest) = E (read rest)
parse ('S' : rest) = S (read rest)
parse ('W' : rest) = W (read rest)
parse ('R' : rest) = R (read rest `div` 90)
parse ('L' : rest) = L (read rest `div` 90)
parse ('F' : rest) = F (read rest)
parse _ = error "Invalid instruction"

type Pos = (Int, Int)

run :: [Instruction] -> (Pos, Pos) -> (Pos, Pos)
run [] = id
run (i : is) = run is . apply i

apply :: Instruction -> (Pos, Pos) -> (Pos, Pos)
apply (N n) ((x, y), p) = ((x, y + n), p)
apply (S n) ((x, y), p) = ((x, y - n), p)
apply (E n) ((x, y), p) = ((x + n, y), p)
apply (W n) ((x, y), p) = ((x - n, y), p)
apply (L 0) h = h
apply (L n) (w, p) = apply (L $ n - 1) (l w, p)
apply (R 0) h = h
apply (R n) (w, p) = apply (R $ n - 1) (r w, p)
apply (F n) ((dx, dy), (x, y)) = ((dx, dy), (x + dx * n, y + dy * n))

main :: IO ()
main = do
    instructions <- fmap parse . lines <$> getContents
    let (_, (x, y)) = run instructions ((10, 1), (0, 0)) in
        print $ abs x + abs y
