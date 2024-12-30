import Text.Parsec
import Control.Monad
import Data.List
import Data.IntMap.Strict (IntMap, (!))
import Data.IntMap.Strict qualified as IntMap

data Tile = Tile { top :: String, right :: String, bottom :: String, left :: String } deriving (Show)

makeTile grid = Tile (head grid) (last dirg) (last grid) (head dirg)
  where dirg = transpose grid

rotations tile = take 4 $ iterate rotate tile

flips tile@(Tile t r b l) = [tile, Tile (reverse t) l (reverse b) r]

rotate (Tile t r b l) = Tile (reverse l) t (reverse r) b

parseTile = do
  string "Tile "
  id :: Int <- read <$> many digit
  char ':'
  newline
  rows <- count 10 (oneOf ".#") `endBy` newline
  return (id, makeTile rows)

parseTiles = parseTile `endBy` newline

solutions :: Int -> Int -> IntMap (Int, Tile) -> IntMap Tile -> [IntMap (Int, Tile)]
solutions _ _ grid tiles | IntMap.null tiles = return grid
solutions size at grid tiles = do
  (id, tile) <- IntMap.assocs tiles
  rotn <- flips =<< rotations tile
  guard $ at `mod` size == 0 || left rotn == (right $ snd $ grid ! (at - 1))
  guard $ at < size || top rotn == (bottom $ snd $ grid ! (at - size))
  solutions size (at + 1) (IntMap.insert at (id, rotn) grid) (IntMap.delete id tiles)

answer contents = head $ do
  solution <- solutions size 0 IntMap.empty tiles
  return $ (fst $ solution ! 0)
    * (fst $ solution ! (size - 1))
    * (fst $ solution ! (size * size - size))
    * (fst $ solution ! (size * size - 1))
  where
    Right tiles = IntMap.fromList <$> parse parseTiles "" contents
    size = round $ sqrt $ fromIntegral $ length tiles

main = getContents >>= print . answer
