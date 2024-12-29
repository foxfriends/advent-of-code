import Control.Arrow
import Data.Function
import Data.Functor
import Data.Map (fromList, (!))

type Pos = (Int, Int)

npos '7' = (0, 0)
npos '8' = (0, 1)
npos '9' = (0, 2)
npos '4' = (1, 0)
npos '5' = (1, 1)
npos '6' = (1, 2)
npos '1' = (2, 0)
npos '2' = (2, 1)
npos '3' = (2, 2)
npos '0' = (3, 1)
npos 'A' = (3, 2)

dpos '^' = (0, 1)
dpos 'A' = (0, 2)
dpos '<' = (1, 0)
dpos 'v' = (1, 1)
dpos '>' = (1, 2)

ysym dy
  | dy < 0 = '^'
  | otherwise = 'v'

xsym dx
  | dx < 0 = '<'
  | otherwise = '>'

path ban (y1, x1) (y2, x2) =
  if ban == (y1, x2) || (x2 > x1 && ban /= (y2, x1)) then yseq ++ xseq ++ "A" else xseq ++ yseq ++ "A"
  where
    yseq = uncurry replicate $ (abs &&& ysym) (y2 - y1)
    xseq = uncurry replicate $ (abs &&& xsym) (x2 - x1)

expand mapping seq = fmap mapping ('A' : seq) `zip` fmap mapping seq

numeric :: String -> Int
numeric seq = expand npos seq >>= uncurry (path (3, 0)) & expand dpos <&> cost & sum
  where
    cost = iterate directional length !! 25

directional :: ((Pos, Pos) -> Int) -> (Pos, Pos) -> Int
directional subcost = memoized
  where
    memoized = sum . fmap (costs !) . expand dpos . uncurry (path (0, 0))
    costs = fromList [((from, to), subcost (from, to)) | from <- dpos <$> "^<v>A", to <- dpos <$> "^<v>A"]

answer = sum . fmap (uncurry (*) . (read . take 3 &&& numeric)) . lines

main = getContents >>= print . answer
