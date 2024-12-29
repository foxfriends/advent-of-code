import Data.Function
import Data.List
import Data.Maybe
import Text.Parsec

int :: Parsec String u Int
int = read <$> many1 digit

parseBuses = ((Just <$> int) <|> (char 'x' >> return Nothing)) `sepBy` char ','

answer :: [Maybe Int] -> Int
answer buses = answer_ offset distance $ tail toFind
  where
    toFind = catMaybes $ zipWith (\i b -> (i,) <$> b) [0 :: Int ..] buses
    (offset, distance) = head toFind

    answer_ n m [] = m - n
    answer_ n d all@((i, m) : rest)
      | n `mod` m == i `mod` m = answer_ n (d * m) rest
      | otherwise = answer_ (n + d) d all

main :: IO ()
main = do
  [_, busstr] <- lines <$> getContents
  let Right buses = parse parseBuses "" busstr
   in print $ answer buses
