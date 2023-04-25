module TsodingParse where

import Data.Char
import Control.Applicative

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Integer
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
               deriving (Show, Eq)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
    fmap f (Parser a) = Parser $ \inp -> do (x, inp') <- a inp
                                            return (f x, inp')

instance Applicative Parser where
    pure x = Parser $ \inp -> Just (x, inp)
    Parser f <*> Parser x  = Parser $ \inp -> do (fs, inp') <- f inp
                                                 (y, inp'') <- x inp'
                                                 return (fs y, inp'')

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
    return = pure
    x >>= f = Parser $ \inp -> do
        (v, out) <- runParser x inp
        runParser (f v) out


jsonNull :: Parser JsonValue
jsonNull = fmap (\_ -> JsonNull) (stringP "null")

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ stringP "true"
    jsonFalse = JsonBool False <$ stringP "false"

jsonNum :: Parser JsonValue
jsonNum = f <$> notNull (many digit)
    where f ds = JsonNumber $ read ds

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> sqBrack elements
        where elements = sepBy (ws *> charP ',' <* ws) jsonValue
              ws = many $ charP ' '

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> cuBrack (sepBy (ws *> charP ',' <* ws) pair)
            where pair = liftA2 (,) (stringLiteral <* ws <* charP ':' <* ws) jsonValue
                  ws = many $ charP ' '


stringLiteral :: Parser String
stringLiteral = charP '\"' *> many (charP ' ')
                           *> many (sat (/= '\"'))
                           <* many (charP ' ') <* charP '\"'


jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNum <|> jsonString 
             <|> jsonArray <|> jsonObject

zero :: Parser a
zero =  Parser $ \_ -> Nothing

item :: Parser Char
item = Parser f
    where f [] = Nothing
          f (x:xs) = Just (x,xs)

charP :: Char -> Parser Char
charP x = Parser f
        where f [] = Nothing
              f (y:ys)
                 | y == x = Just (x,ys)
                 | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
        if p x then return x else zero

digit :: Parser Char
digit = sat isDigit

sepBy :: Parser a -> Parser b -> Parser [b]
sep `sepBy` el = (:) <$> el <*> many (sep *> el) <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \inp -> do
        (xs, inp') <- p inp
        if null xs
            then Nothing
            else Just (xs, inp')

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do _ <- open
                          x <- ws *> p <* ws
                          _ <- close
                          return x
                    where ws =  many (charP ' ')

sqBrack :: Parser a -> Parser a
sqBrack p = bracket (charP '[') p (charP ']')

cuBrack :: Parser a -> Parser a
cuBrack p = bracket (charP '{') p (charP '}')

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    return (fst <$> runParser parser input)

main :: IO ()
main = undefined