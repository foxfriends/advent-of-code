import Data.List ((!?))
import Text.Parsec

surrounding x y =
  [ (x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)
  ]

at grid x y = case ((grid !? y) >>= (!? x)) of
  Just ch -> ch
  Nothing -> '.'

isBlocked grid (x, y) = at grid x y == '@'

clean grid = if cleaned == grid then grid else clean cleaned
  where
    cleaned = [[if at grid x y == '.' || (length $ filter (isBlocked grid) $ surrounding x y) < 4 then '.' else '@' | x <- [0 .. (length $ grid !! y) - 1]] | y <- [0 .. length grid - 1]]

answer input = init - final
  where
    Right grid = parse ((many $ oneOf "@.") `sepEndBy` char '\n') "" input
    init = length $ filter ((==) '@') input
    final = length $ filter ((==) '@') $ concat $ (clean grid)

main = getContents >>= print . answer
