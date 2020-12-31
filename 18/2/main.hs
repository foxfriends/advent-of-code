{-# LANGUAGE OverloadedStrings, LambdaCase #-}
import Control.Monad.State
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Read

data Op = Add | Mul

eval :: Text -> Int
eval = evalState factors

number :: State Text Int
number = do
    e <- gets (decimal . T.strip)
    case e of
        Left _ -> do
            e <- get
            error $ T.unpack e
        Right (num, str) -> do
            put $ T.strip str
            return num

parenthesized :: State Text Int
parenthesized = do
    modify (T.strip . T.tail)
    x <- factors
    modify (T.strip . T.tail)
    return x

getValue :: State Text Int
getValue = do
    str <- gets T.strip
    if "(" `T.isPrefixOf` str
        then parenthesized
        else number

mapsnd :: (b -> c) -> (a, b) -> (a, c)
mapsnd f (a, b) = (a, f b)

op :: Char -> State Text Bool
op c = do
    mc <- gets $ fmap (mapsnd T.strip) . T.uncons
    case mc of
        Just (cx, cs) | cx == c -> do
            put cs
            return True
        _ -> return False

add = op '+'
mul = op '*'

terms :: State Text Int
terms = do
    lhs <- getValue
    add >>= \case
        False -> return lhs
        True -> (lhs +) <$> terms

factors :: State Text Int
factors = do
    lhs <- terms
    mul >>= \case
        False -> return lhs
        True -> (lhs *) <$> factors

main :: IO ()
main = getContents >>= print . sum . fmap eval . T.lines . T.pack
