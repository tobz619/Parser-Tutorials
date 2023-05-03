module RunParser where

import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Control.Applicative
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T


type Parser = Parsec Void Text

something = parseTest (satisfy (== 'a') :: Parser Char) ""

parseA = parseTest (satisfy (== 'a') :: Parser Char) "a"

parseChar x = \inp -> parseTest (char x :: Parser Char) inp

newline = single '\n' :: Parser Char

mySequenceA :: Parser (Char, Char, Char)
mySequenceA = (,,) <$> char 'a' <*> char 'b' <*> char 'c'

mySequenceM :: Parser (Char, Char, Char)
mySequenceM = do a <- char 'a'
                 b <- char 'b'
                 c <- char 'c'
                 return (a,b,c)

manyAs :: Parser String
manyAs = many (char 'a') :: Parser String

manyChain :: Char -> Parser String
manyChain x = many (char x) :: Parser String

stopMany :: Char -> Parser String
stopMany x = many (char x) <* eof

-- From now on we will be developing a real, useful parser that can parse URIs of the following form:
--
-- scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]

data Scheme = SchemeData | SchemeFile | SchemeFtp | SchemeHttps
            | SchemeHttp | SchemeIrc | SchemeMailto deriving (Show, Eq, Enum)

pSchemeF :: Parser Text
pSchemeF =  string "data" <|> string "file" <|> string "ftp" <|>
           string "http" <|> string "https" <|> string "irc" <|>
           string "mailto"

pSchemeC :: Parser Text
pSchemeC = choice $ string <$> ["data","file","ftp","http","https","irc","mailto"]

getScheme :: Parser Scheme
getScheme = choice $ zipWith (<$) [SchemeData .. SchemeMailto] ["data","file","ftp","https","http","irc","mailto"]

data Uri = Uri { uriScheme :: Scheme
               , uriAuthority :: Maybe Authority
               , uriPath :: Text
               , uriQuery :: Maybe Query
               , uriFragment :: Maybe Fragment
               }
               deriving (Show, Eq)

data Authority = Authority { authUser :: Maybe (Text, Text)
                           , authHost :: Text
                           , authPort :: Maybe Int }
                           deriving (Show, Eq)

-- | Parses a scheme and immediately checks for a comma.
-- >>> parseTest pUri "irc"
-- 1:4:
--   |
-- 1 | irc
--   |    ^
-- unexpected end of input
-- expecting ':'
-- >>> parseTest pUri "irc:"
-- Uri {uriScheme = "irc"}
pUri :: Parser Uri
pUri = (Uri <$> getScheme <* char ':'
           <*> getUriAuthA
           <*> (getSlash *> getPath)
           <*> getQuery
           <*> getFragment) <*eof

getUriAuth :: Parser (Maybe Authority)
getUriAuth = optional $ do 
    void (string "//")
    authUser <- optional . try $ do              -- (2)
      user <- T.pack <$> some alphaNumChar       -- (3)
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal) -- (4)
    return (Authority authUser authHost authPort)    -- (5)

getUriAuthA :: Parser (Maybe Authority)
getUriAuthA = optional . try $ Authority <$> user <*> host <*> port
            where user = string "//" *> (optional . try $ do
                            user <- T.pack <$> some alphaNumChar
                            _ <- char ':'
                            password <- T.pack <$> some alphaNumChar
                            _ <- char '@'
                            return (user, password))
                  host = T.pack <$> some (alphaNumChar <|> char '.')
                  port = optional (char ':' *> L.decimal) 

getSlash :: Parser (Maybe Char)
getSlash = optional $ char '/'

getPath :: Parser Text
getPath =  T.pack <$> many alphaNumChar

newtype Query = Query Text deriving (Show, Eq)

getQuery :: Parser (Maybe Query)
getQuery = optional $ Query <$> (T.pack <$> (char '?' *> many alphaNumChar))

newtype Fragment = Fragment Text deriving (Show, Eq)

getFragment :: Parser (Maybe Fragment)
getFragment = optional $ Fragment <$> (T.pack <$> (char '#' *> many alphaNumChar))

type ParState =  ParsecT Void Text (State String)

parser0 :: ParState String
parser0 = a <|> b
      where a = "foo" <$ put "branch A"
            b = get <* put "branch B"


parser1 :: ParState String
parser1 = a <|> b
      where a = "foo" <$ put "branch A" <* empty
            b = get <* put "branch B"

aFunc :: IO ()
aFunc = do
  let run p          = runState (runParserT p "" "") "initial"
      (Right a0, s0) = run parser0
      (Right a1, s1) = run parser1

  putStrLn  "Parser 0"
  putStrLn ("Result:      " ++ show a0)
  putStrLn ("Final state: " ++ show s0)

  putStrLn  "Parser 1"
  putStrLn ("Result:      " ++ show a1)
  putStrLn ("Final state: " ++ show s1)

-- >>> aFunc

type ParState0 = StateT String (ParsecT Void Text Identity)

parser3 :: ParState0 String
parser3 = a <|> b
  where
    a = "foo" <$ put "branch A" <* empty
    b = get   <* put "branch B"

aFunc2 :: IO ()
aFunc2 = do
  let p            = runStateT parser3 "initial"
      Right (a, s) = runParser p "" ""
  putStrLn ("Result:      " ++ show a)
  putStrLn ("Final state: " ++ show s)

-- >>> aFunc2

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\'')

-- manyTill :: Alternative m => m a -> m end -> m [a]
-- manyTill p end = go
--   where
--     go = ([] <$ end) <|> ((:) <$> p <*> go)

integer :: Parser Int
integer = lexeme L.decimal

data Expr =  Var String | Int Int | Negation Expr
           | Sum Expr Expr | Subtr Expr Expr | Product Expr Expr
           | Div Expr Expr deriving (Eq, Ord, Show)

pVariable :: Parser Expr
pVariable = Var <$> lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInt :: Parser Expr
pInt = Int <$> integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = parens pExpr <|> pVariable <|> pInt

pExpr :: Parser Expr
pExpr = makeExprParser pTerm opsTable

opsTable :: [[Operator Parser Expr]]
opsTable = [ [prefix "-" Negation , prefix "+" id]
           , [binary "*" Product  , binary "/" Div]
           , [binary "+" Sum      , binary "-" Subtr] ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)

exprToInt :: Expr -> Maybe Integer
exprToInt (Int i) = pure (toInteger i)
exprToInt (Sum v1 v2) = (+) <$> exprToInt v1 <*> exprToInt v2
exprToInt (Product v1 v2) = (*) <$> exprToInt v1 <*> exprToInt v2
exprToInt (Div v1 v2)
 | exprToInt v2 == Just 0 = Nothing
 | otherwise = div <$> exprToInt v1 <*> exprToInt v2
exprToInt (Negation v1) = negate <$> exprToInt v1
exprToInt (Subtr v1 v2) = subtract <$> exprToInt v1 <*> exprToInt v2
exprToInt _ = Nothing