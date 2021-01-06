{-# LANGUAGE LambdaCase, BlockArguments, ViewPatterns, TupleSections #-}
import Control.Monad.State.Lazy
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.Functor
import Data.Char
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe

type Assoc k v = [(k, v)]

data Expansion = Terminal Char | NonTerminal [Int] deriving (Show)
type Grammar = Assoc Int Expansion

recover :: StateT String Maybe t -> StateT String Maybe t
recover f = do
    state <- get
    catchError f (\_ -> put state >> throwError ())

(<|>) :: StateT String Maybe a -> StateT String Maybe a -> StateT String Maybe a
(<|>) f g = catchError (recover f) (const g)

grammar :: Monad m => StateT [String] m Grammar
grammar = do
    line <- state (fromJust . uncons)
    if null line then return [] else
        let (from, to) = rule line in do
            (++) ((from, ) <$> to) <$> grammar

number :: StateT String Maybe Int
number = do
    str <- state $ span isDigit
    if null str
        then throwError ()
        else return $ read str

terminal :: StateT String Maybe Char
terminal = do
    gets (stripPrefix "\"") >>= lift >>= put
    c <- state (fromJust . uncons)
    gets (stripPrefix "\"") >>= lift >>= put
    return c

separated :: String -> StateT String Maybe t -> StateT String Maybe [t]
separated separator parser = do
    lhs <- parser
    rhs <- recover (exactly separator *> separated separator parser) <|> return []
    return $ lhs : rhs

exactly :: String -> StateT String Maybe ()
exactly prefix = recover $ gets (stripPrefix prefix) >>= lift >>= put

expansion :: StateT String Maybe Expansion
expansion
    = (Terminal <$> terminal)
    <|> (NonTerminal <$> separated " " number)

rule :: String -> (Int, [Expansion])
rule = fromJust . evalStateT ((,) <$> number <*> (exactly ": " *> separated " | " expansion))

valid :: Grammar -> [String]
valid = (! 0) . valid_ Map.empty
    where
        valid_ :: Map Int [String] -> Grammar -> Map Int [String]
        valid_ s [] = s
        valid_ s g = let (now, later) = partition (ready s) g in
            valid_ (foldl' add s now) later

        ready :: Map Int a -> (Int, Expansion) -> Bool
        ready _ (_, Terminal _) = True
        ready s (_, NonTerminal xs) = all (`Map.member` s) xs

        prev = Map.findWithDefault []

        add :: Map Int [String] -> (Int, Expansion) -> Map Int [String]
        add s (i, NonTerminal xs) = Map.insert i (prev i s `union` foldl1' (\l r -> [ll ++ rr | ll <- l, rr <- r]) ((s !) <$> xs)) s
        add s (i, Terminal c) = Map.insert i [[c]] s

run :: [String] -> IO Int
run = evalStateT run_
    where
        run_ :: StateT [String] IO Int
        run_ = do
            g <- grammar
            ms <- get
            let v = valid g in do
                return . length $ filter (`elem` valid g) ms

main :: IO ()
main = getContents >>= run . lines >>= print
