import Data.Char (ord)
import Data.List (break, elemIndex, nub)
import Data.Maybe (fromJust)

data Grid t = Grid [t] Int Int

data Position = Position Int Int deriving (Eq)

toPosition :: Int -> Int -> Position
toPosition width index = Position (index `mod` width) (index `div` width)

positionOf :: Position -> Int -> Int
positionOf (Position x y) w = x + y * w

at :: Grid a -> Position -> a
at (Grid list w _) position = list !! (positionOf position w)

setAt :: Position -> a -> Grid a -> Grid a
setAt position val (Grid l w h) = Grid (pref ++ val : rest) w h
  where
    index = positionOf position w
    (pref, _ : rest) = splitAt index l

within :: Grid a -> Position -> Bool
within (Grid _ w h) (Position x y) = x >= 0 && x < w && y >= 0 && y < h

mapHeight :: Char -> Char
mapHeight 'S' = 'a'
mapHeight 'E' = 'z'
mapHeight c = c

split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x list = start : split x rest
  where
    (start, _ : rest) = break (== x) list

above (Position x y) = Position x (y - 1)

below (Position x y) = Position x (y + 1)

toRight (Position x y) = Position (x + 1) y

toLeft (Position x y) = Position (x - 1) y

adjacent position = [above, below, toRight, toLeft] <*> [position]

reachable :: Char -> Char -> Bool
reachable from to = ord to <= 1 + ord from

doClimb :: Grid Char -> Grid Int -> [Position] -> Grid Int
doClimb grid distances [] = distances
doClimb grid distances (pos : queue) = do
  let distance = 1 + distances `at` pos
      altitude = grid `at` pos
      nextPositions = filter (within grid) $ adjacent pos
      accessible = filter (reachable altitude . at grid) nextPositions
      improvement = filter ((distance <) . at distances) accessible
      newDistances = foldl (\g p -> setAt p distance g) distances improvement
   in doClimb grid newDistances (nub $ queue ++ improvement)

climb :: Grid Char -> Position -> Int
climb grid@(Grid t w h) end = distances `at` end
  where
    initial = Grid (fmap (\a -> if a == 'a' then 0 else maxBound) t) w h
    starts = map (toPosition w . fst) $ filter ((==) 'a' . snd) $ zip [0 ..] t
    distances = doClimb grid initial starts

inputLines :: IO [String]
inputLines = filter (not . null) <$> split '\n' <$> getContents

parseInput :: [String] -> (Grid Char, Position)
parseInput input =
  ( Grid (mapHeight <$> list) width height,
    toPosition width endIndex
  )
  where
    height = length input
    list = concat input
    width = (length list) `div` height
    endIndex = fromJust $ elemIndex 'E' list

main :: IO ()
main = do
  (grid, end) <- parseInput <$> inputLines
  print $ climb grid end
