import Control.Monad

data Seat = Occupied | Unoccupied deriving (Eq)

seat '.' = Nothing
seat 'L' = Just Unoccupied
seat '#' = Just Occupied
seat _ = error "invalid seat"

isOccupied :: Maybe Seat -> Bool
isOccupied (Just Occupied) = True
isOccupied _ = False

compute :: [[Maybe Seat]] -> Int
compute seats = if seats == nextSeats
    then length $ filter isOccupied (join seats)
    else compute nextSeats
    where
        nextSeats = fmap (uncurry updateRow) ([0..] `zip` seats)
        updateRow i row = fmap (uncurry (updateSeat i)) ([0..] `zip` row)
        updateSeat _ _ Nothing = Nothing
        updateSeat i j (Just seat) = case length $ filter isOccupied $ adjacent i j of
            0 -> Just Occupied
            l | l >= 4 -> Just Unoccupied
            _ -> Just seat
        adjacent i j = [seats !! y !! x | y <- [i-1..i+1], x <- [j-1..j+1], x >= 0, y >= 0, y < length seats, x < length (seats !! y), (y, x) /= (i, j)]

main :: IO ()
main = do
    seats <- fmap (fmap seat) . lines <$> getContents
    print $ compute seats
