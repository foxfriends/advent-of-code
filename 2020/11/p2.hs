import Control.Monad
import Data.Functor
import Data.List
import Data.Maybe

data Seat = Occupied | Unoccupied deriving (Eq, Show)

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
        updateSeat i j (Just seat) = case length $ filter isOccupied $ visible i j of
            0 -> Just Occupied
            l | l >= 5 -> Just Unoccupied
            _ -> Just seat
        visible i j = join . find isJust <$> takeRay i j <$> directions
        directions = [(dy, dx) | dy <- [-1..1], dx <- [-1..1], (dx, dy) /= (0, 0)]
        takeRay :: Int -> Int -> (Int, Int) -> [Maybe Seat]
        takeRay y x (dy, dx) = fromMaybe [] (seats !? ny >>= (!? nx) <&> (: takeRay ny nx (dy, dx)))
            where ny = y + dy
                  nx = x + dx

main :: IO ()
main = do
    seats <- fmap (fmap seat) . lines <$> getContents
    print $ compute seats
