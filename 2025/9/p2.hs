import Text.Parsec
import Data.List (sortBy, sort)
import Control.Monad (guard)

integer = read <$> many digit
point = do
  x :: Int <- integer
  char ','
  y :: Int <- integer
  return (x, y)

area ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

isHorizontal ((_, y1), (_, y2)) = y1 == y2
isVertical ((x1, _), (x2, _)) = x1 == x2

collide (p1, p2) ((bx1, by1), (bx2, by2)) =
  let
    (px1, px2) = if fst p1 < fst p2 then (fst p1, fst p2) else (fst p2, fst p1)
    (py1, py2) = if snd p1 < snd p2 then (snd p1, snd p2) else (snd p2, snd p1)
  in px1 < bx2 && px2 > bx1 && py1 < by2 && py2 > by1

containsBy key (a, b) (p1, p2) =
  let (w, z) = if key p1 < key p2 then (key p1, key p2) else (key p2, key p1) in
  w <= a && a <= z && w <= b && b <= z

answer :: String -> Int
answer input = best
  where
    Right points = parse (point `sepEndBy` char '\n') "" input
    edges = zip ((last points) : points) points

    verticals = sortBy (\a b -> compare (fst $ fst a) (fst $ fst b)) $ filter isVertical edges
    horizontals = sortBy (\a b -> compare (snd $ fst a) (snd $ fst b)) $ filter isHorizontal edges

    xs = (fst.fst) <$> verticals
    ys = (snd.fst) <$> horizontals

    min_x = head xs
    max_x = last xs
    min_y = head ys
    max_y = last ys

    y_ranges = zip ys (tail ys)
    x_ranges = zip xs (tail xs)

    illegal_x = do
      range@(y1, y2) <- y_ranges
      guard (y1 /= y2)
      let
        xs = (fst . fst) <$> filter (containsBy snd range) verticals
        x1 = head xs
        x2 = last xs
        in [((min_x, y1), (x1, y2)), ((x2, y1), (max_x, y2))]

    illegal_y = do
      range@(x1, x2) <- x_ranges
      guard (x1 /= x2)
      let
        ys = (snd . fst) <$> filter (containsBy fst range) horizontals
        y1 = head ys
        y2 = last ys
        in [((x1, min_y),(x2, y1)), ((x1, y2),(x2, max_y))]
    illegal = illegal_x ++ illegal_y
    best :: Int = foldl max 0
      $ fmap area
      $ filter (\point -> not $ any (collide point) illegal)
      $ [(points !! i, points !! j) | i <- [0..length points - 1], j <- [i + 1..length points - 1]]

main = getContents >>= print . answer
