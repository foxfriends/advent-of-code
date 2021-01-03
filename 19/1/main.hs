{-# LANGUAGE LambdaCase, BlockArguments, ViewPatterns #-}
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Functor
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Expansion = Terminal Char | NonTerminal [Int] deriving (Show)
type Grammar = Map Int [Expansion]
type NFA = Map (Int, Char) [Int]
newtype Parser t = Parser (State (Grammar, [Int], String) t)

grammar :: Monad m => StateT [String] m Grammar
grammar = do
    line <- state (fromJust . uncons)
    if null line then return Map.empty else
        let (from, to) = rule line in do
            Map.insert from to <$> grammar

number :: StateT String Maybe Int
number = recover do
    str <- state $ span isDigit
    if null str
        then throwError ()
        else return $ read str

recover :: StateT String Maybe t -> StateT String Maybe t
recover f = do
    state <- get
    catchError f (\_ -> put state >> throwError ())

terminal :: StateT String Maybe Char
terminal = recover do
    gets (stripPrefix "\"") >>= lift >>= put
    c <- state (fromJust . uncons)
    gets (stripPrefix "\"") >>= lift >>= put
    return c

separated :: String -> StateT String Maybe t -> StateT String Maybe [t]
separated separator parser = recover do
    lhs <- parser
    rhs <- recover (exactly separator *> separated separator parser) <|> return []
    return $ lhs : rhs

exactly :: String -> StateT String Maybe ()
exactly prefix = recover $ gets (stripPrefix prefix) >>= lift >>= put

(<|>) :: MonadError e m => m a -> m a -> m a
(<|>) f g = catchError f (const g)

expansion :: StateT String Maybe Expansion
expansion
    = (Terminal <$> terminal)
    <|> (NonTerminal <$> separated " " number)

rule :: String -> (Int, [Expansion])
rule = fromJust . evalStateT ((,) <$> number <*> (exactly ": " *> separated " | " expansion))

partitionMap :: Traversable t => (a -> Either b c) -> t a -> (t b, t c)
partitionMap f t =

type Transition = (Int, Char, Int)

nfa :: Grammar -> NFA
nfa g = nfa_ Map.empty g
    where
        nfa_ :: NFA -> Grammar -> NFA
        nfa_ n (null -> True) = n
        nfa_ n g = let (rest, transitions) = partitionMap (connects n) <$> assocs g in
            nfa_ n rest

        connects :: NFA -> Int -> Expansion -> Maybe (Either (Int, [Expansion]) Transition)
        connects n k (Terminal c) = Just . Right (-1, c, k)
        connects n k v@(NonTerminal s) = find (elem s . snd) (assocs n) <&> \(i, c, _) -> Right (s, c, k)

run :: [String] -> IO Int
run = evalStateT run_
    where
        run_ :: StateT [String] IO Int
        run_ = do
            g <- grammar
            n <- nfa g
            ms <- get
            error ""

main :: IO ()
main = getContents >>= run . lines >>= print
