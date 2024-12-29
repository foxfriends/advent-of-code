{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
import Control.Monad
import Data.List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)

split :: Eq a => [a] -> [a] -> [[a]]
split x ys = splitInner x [] ys
    where
        splitInner _ [] [] = []
        splitInner _ r [] = [reverse r]
        splitInner x r (stripPrefix x -> Just ys) = reverse r : splitInner x [] ys
        splitInner x r (y : ys) = splitInner x (y : r) ys

data Field = Field Text [Int] deriving (Eq, Show)
fieldRange (Field _ ranges) = ranges
fieldName (Field name _) = name

newtype Ticket = Ticket [Int]
ticketFields (Ticket fields) = fields

range :: Text -> [Int]
range = containing . fmap (read . T.unpack) . T.splitOn "-"
    where containing [low, hi] = [low..hi]

field :: Text -> Field
field = makeField . T.splitOn ":"
    where makeField [name, ranges] = Field name (range . T.strip =<< T.splitOn "or" ranges)

ticket :: Text -> Ticket
ticket = Ticket . fmap (read . T.unpack) . T.splitOn ","

tickets :: [Text] -> [Ticket]
tickets = fmap ticket

parse :: Text -> ([Field], Ticket, [Ticket])
parse input = (fmap field f, head $ tickets $ tail t, tickets $ tail ts)
    where [f, t, ts] = split [""] $ T.lines input

elemOf :: Eq a => [a] -> a -> Bool
elemOf = flip elem

removeInvalid :: [Field] -> [Ticket] -> [Ticket]
removeInvalid fields = filter (all (`elem` (fields >>= fieldRange)) . ticketFields)

findCandidates :: [Field] -> [Int] -> [Field]
findCandidates fields xs = filter (\f -> all (`elem` fieldRange f) xs) fields

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

only :: [a] -> a
only [a] = a
only _ = error "Not the only one"

assignFields :: [(Int, [Field])] -> Map Int Field
assignFields = only . assignFields_ Map.empty
    where removeField f = sortOn (length . snd) . fmap (mapSnd (delete f))

          assignFields_ :: Map Int Field -> [(Int, [Field])] -> [Map Int Field]
          assignFields_ m [] = [m]
          assignFields_ m ((_, []) : _) = []
          assignFields_ m ((i, fs) : cs) = fs >>= \f -> assignFields_ (Map.insert i f m) $ removeField f cs

main :: IO ()
main = do
    (fields, Ticket myTicket, nearbyTickets) <- parse . T.pack <$> getContents
    let
        validTickets = removeInvalid fields nearbyTickets
        fieldValues = transpose (ticketFields <$> validTickets)
        candidates = zip [0..] (findCandidates fields <$> fieldValues)
        fieldAssignment = assignFields (sortOn (length . snd) candidates)
        departureFields = Map.filter (T.isPrefixOf "departure") (fieldName <$> fieldAssignment)
        in print $ product $ (myTicket !!) <$> Map.keys departureFields
