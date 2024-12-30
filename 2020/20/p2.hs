import Text.Parsec
import Data.Bits
import Control.Monad
import Data.List
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap

data Tile = Tile { image :: [String], top :: String, right :: String, bottom :: String, left :: String }

makeTile grid = Tile (stripBorder grid) (head grid) (last dirg) (last grid) (head dirg)
  where dirg = transpose grid

rotations tile = take 4 $ iterate rotateTile tile

flips tile@(Tile i t r b l) = [tile, Tile (reverse <$> i) (reverse t) l (reverse b) r]

rotateTile (Tile i t r b l) = Tile (rotateGrid i) (reverse l) t (reverse r) b

rotateGrid = transpose . reverse

parseTile = do
  string "Tile "
  id :: Int <- read <$> many digit
  char ':'
  newline
  rows <- count 10 (oneOf ".#") `endBy` newline
  return (id, makeTile rows)

parseTiles = parseTile `endBy` newline

solutions :: Int -> Int -> IntMap Tile -> IntMap Tile -> [IntMap Tile]
solutions _ _ grid tiles | IntMap.null tiles = return grid
solutions size at grid tiles = do
  (id, tile) <- IntMap.assocs tiles
  rotn <- flips =<< rotations tile
  guard $ at `mod` size == 0 || left rotn == (right $ grid ! (at - 1))
  guard $ at < size || top rotn == (bottom $ grid ! (at - size))
  solutions size (at + 1) (IntMap.insert at rotn grid) (IntMap.delete id tiles)

windows size [] = []
windows size xs = group : windows size rest
  where (group, rest) = splitAt size xs

seamonster = bitmap <$>
  [ "                  # "
  , "#    ##    ##    ###"
  , " #  #  #  #  #  #   "
  ]

stripBorder = fmap (init . tail) . init . tail

bitmap :: String -> Integer
bitmap = foldr pushbit 0
  where
    pushbit '#' n = n `shiftL` 1 + 1
    pushbit '.' n = n `shiftL` 1
    pushbit ' ' n = n `shiftL` 1

findSeaMonsters :: [Integer] -> [Integer]
findSeaMonsters image@(_:_:_:_) = a : (b .|. b2) : (c .|. c2) : rest
  where
    [a,b,c] = scanRows (take 3 image)
    (b2:c2:rest) = findSeaMonsters (tail image)
    scanRows :: [Integer] -> [Integer]
    scanRows rows
      | any (== 0) rows = [0, 0, 0]
      | isMonster rows = zipWith (.|.) seamonster $ scanRest
      | otherwise = scanRest
      where scanRest = fmap (`shiftL` 1) $ scanRows $ fmap (`shiftR` 1) rows

    isMonster [a, b, c] = a .&. top == top && b .&. mid == mid && c .&. bot == bot

    [top, mid, bot] = seamonster
findSeaMonsters _ = [0, 0]

population :: [String] -> Int
population = length . filter (== '#') . concat

answer :: String -> Int
answer contents = population fullimage - seamonsters
  where
    Right tiles = IntMap.fromList <$> parse parseTiles "" contents
    size = round $ sqrt $ fromIntegral $ length tiles
    fullimage = concat $ fmap (fmap concat . transpose) $ windows size $ fmap image $ IntMap.elems $ head $ solutions size 0 IntMap.empty tiles
    seamonsters = maximum $ fmap (sum . fmap popCount . findSeaMonsters . fmap bitmap) $ take 4 $ iterate rotateGrid $ fullimage

main = getContents >>= print . answer
