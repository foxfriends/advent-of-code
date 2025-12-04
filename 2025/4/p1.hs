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

answer input =
  length
    [ 1 | y <- [0 .. length grid - 1],
          x <- [0 .. (length $ grid !! y) - 1],
          grid !! y !! x == '@',
          length [1 | (x2, y2) <- surrounding x y,
                      ((grid !? y2) >>= (!? x2)) == (Just '@')] < 4
    ]
  where
    Right grid = parse ((many $ oneOf "@.") `sepEndBy` char '\n') "" input

main = getContents >>= print . answer
