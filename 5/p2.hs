import Data.List

main :: IO ()
main = do
    contents <- getContents
    let
        seats = sort $ fmap seatID (lines contents)
        Just (before, _) = find (\(x, y) -> x + 2 == y) (zip seats (tail seats))
        in print $ before + 1

seatID :: String -> Int
seatID s = let (row, col) = rowcol s in row * 8 + col

rowcol :: String -> (Int, Int)
rowcol = rowcol_ [0..127] [0..8]
    where
        rowcol_ row col [] = (head row, head col)
        rowcol_ row col s = case s of
            'F' : s -> rowcol_ (take halfrow row) col s
            'B' : s -> rowcol_ (drop halfrow row) col s
            'L' : s -> rowcol_ row (take halfcol col) s
            'R' : s -> rowcol_ row (drop halfcol col) s
            _ -> error "wrong"
            where halfrow = length row `div` 2
                  halfcol = length col `div` 2
