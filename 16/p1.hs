{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
import Data.List
import qualified Data.Text as T
import Data.Text (Text)

split :: Eq a => [a] -> [a] -> [[a]]
split x ys = splitInner x [] ys
    where
        splitInner _ [] [] = []
        splitInner _ r [] = [reverse r]
        splitInner x r (stripPrefix x -> Just ys) = reverse r : splitInner x [] ys
        splitInner x r (y : ys) = splitInner x (y : r) ys

data Field = Field Text [[Int]]
fieldRanges (Field _ ranges) = ranges

newtype Ticket = Ticket [Int]
ticketFields (Ticket fields) = fields

range :: Text -> [Int]
range = containing . fmap (read . T.unpack) . T.splitOn "-"
    where containing [low, hi] = [low..hi]

field :: Text -> Field
field = makeField . T.splitOn ":"
    where makeField [name, ranges] = Field name (range . T.strip <$> T.splitOn "or" ranges)

ticket :: Text -> Ticket
ticket = Ticket . fmap (read . T.unpack) . T.splitOn ","

tickets :: [Text] -> [Ticket]
tickets = fmap ticket . tail

parse :: Text -> ([Field], Ticket, [Ticket])
parse input = (fmap field f, head $ tickets $ tail t, tickets $ tail ts)
    where [f, t, ts] = split [""] $ T.lines input

main :: IO ()
main = do
    (fields, _, nearbyTickets) <- parse . T.pack <$> getContents
    let ranges = fields >>= fieldRanges
        values = nearbyTickets >>= ticketFields
        in print $ sum $ filter (not . flip any (flip elem <$> ranges) . flip ($)) values
