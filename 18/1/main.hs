{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.State
import Data.Either
import Data.Maybe
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Read

data Op = Add | Mul

eval :: Text -> Int
eval = evalState (eval' 0 Add)

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
    eval' 0 Add

getValue :: State Text Int
getValue = do
    str <- gets T.strip
    if "(" `T.isPrefixOf` str
        then parenthesized
        else number

getOp :: State Text (Maybe Op)
getOp = do
    mc <- gets T.uncons
    case mc of
        Nothing -> return Nothing
        Just (c, cs) -> do
            put $ T.strip cs
            case c of
                '+' -> return $ Just Add
                '*' -> return $ Just Mul
                ')' -> return Nothing
                _ -> error [c]

eval' :: Int -> Op -> State Text Int
eval' lhs Add = do
    rhs <- getValue
    op <- getOp
    let v = lhs + rhs in
        maybe (return v) (eval' v) op
eval' lhs Mul = do
    rhs <- getValue
    op <- getOp
    let v = lhs * rhs in
        maybe (return v) (eval' v) op

main :: IO ()
main = getContents >>= print . sum . fmap eval . T.lines . T.pack
