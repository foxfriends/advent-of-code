{-# LANGUAGE FlexibleInstances #-}
import Control.Monad
import Data.Maybe

tile :: Char -> Bool
tile '#' = True
tile '.' = False

update :: Int -> Bool -> Bool
update 2 True = True
update 3 _ = True
update _ _ = False

class Remake i where
    remake :: i -> i
instance Remake Bool where
    remake = const False
instance (Functor f, Remake i) => Remake (f i) where
    remake = fmap remake

class Expand e where
    expand :: e -> e
instance (Remake e, Expand e) => Expand [e] where
    expand e = remake (head a) : a ++ [remake (last a)]
        where a = fmap expand e
instance Expand Bool where
    expand = id

(!?) :: [a] -> Int -> Maybe a
(!?) l x | x < 0 = Nothing
(!?) l x | x >= length l = Nothing
(!?) l x = Just (l !! x)

cycleOnce :: [[[Bool]]] -> [[[Bool]]]
cycleOnce state = run (expand state)

neighbours :: [[[Bool]]] -> (Int, Int, Int) -> Int
neighbours space (x, y, z) = count [space !? zz >>= (!? yy) >>= (!? xx) | xx <- [x-1..x+1],
                                                                          yy <- [y-1..y+1],
                                                                          zz <- [z-1..z+1],
                                                                          xx /= x || yy /= y || zz /= z]
    where count = length . filter id . catMaybes

run :: [[[Bool]]] -> [[[Bool]]]
run space = [[[next (x, y, z) value | (x, value) <- [0..] `zip` row]
                                    | (y, row) <- [0..] `zip` plane]
                                    | (z, plane) <- [0..] `zip` space]
    where
        next :: (Int, Int, Int) -> Bool -> Bool
        next = update . neighbours space

cycles :: Int -> [[[Bool]]] -> [[[Bool]]]
cycles 0 state = state
cycles n state = cycles (n - 1) $ cycleOnce state

main :: IO ()
main = do
    contents <- fmap (fmap tile) . lines <$> getContents
    print . length . filter id . join . join $ cycles 6 [contents]
