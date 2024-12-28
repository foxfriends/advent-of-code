import Control.Monad
import Data.Either
import Data.IntMap (IntMap, (!))
import Data.IntMap qualified as IntMap
import Text.Parsec

int :: Parsec String u Int
int = read <$> many1 digit

spaceSep = try $ do
  char ' '
  notFollowedBy $ char '|'

nonterminal = NonTerminal <$> int `sepBy1` spaceSep

terminal =
  Terminal <$> do
    char '"'
    ch <- letter
    char '"'
    return ch

data Rule = Terminal Char | NonTerminal [Int]

parseParser = do
  id <- int
  string ": "
  alternatives <- (nonterminal <|> terminal) `sepBy` string " | "
  return (id, alternatives)

buildParser :: IntMap [Rule] -> Parsec String u ()
buildParser map = do
  l <- length <$> many1 (rules ! 42)
  r <- length <$> many1 (rules ! 31)
  eof
  guard $ l > r
  return ()
  where
    rules = fmap ruleParser map
    ruleParser [single] = altParser single
    ruleParser many = choice $ fmap (try . altParser) many
    altParser (Terminal ch) = void $ char ch
    altParser (NonTerminal ids) = foldr (>>) (return ()) $ fmap (rules !) ids

parseInput = do
  rules <- buildParser . IntMap.fromList <$> parseParser `endBy` newline
  newline
  inputs <- many (oneOf "ab") `endBy` newline
  return (rules, inputs)

answer contents = length $ filter isRight $ parse parser "" <$> inputs
  where
    Right (parser, inputs) = parse parseInput "" contents

main :: IO ()
main = getContents >>= print . answer
