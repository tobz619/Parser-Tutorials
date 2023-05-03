module ParserTut where

import Data.Char (ord, isSpace)


import qualified Control.Applicative as Ap

newtype Parser a = Parser {runParser :: String -> [(a,String)]}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \inp -> [(f v, rest) | (v, rest) <- p inp]

instance Applicative Parser where
    pure = return
    Parser f <*> Parser x = Parser $ \inp -> [(func v, inp'') | (func, inp') <- f inp,
                                                                (v, inp'') <- x inp']

instance Monad Parser where
    return = result
    Parser p >>= f = Parser $ \inp -> [res | (v, out) <- p inp, res <- runParser (f v) out]


zero :: Parser a
zero = Parser $ \_ -> []

result :: a -> Parser a
result v = Parser $ \inp -> [(v,inp)]

item :: Parser Char
item = Parser $ \inp -> case inp of
                  []      -> []
                  (x:xs)  -> [(x,xs)]

pseq :: Parser a -> Parser b -> Parser (a,b)
p  `pseq` q = Parser $ \inp -> [( (v,w), inp'') | (v, inp') <- runParser p inp,
                                                  (w, inp'') <- runParser q inp']

seq' :: Parser a -> Parser b -> Parser (a,b)
seq' p q = p >>= \x ->
           q >>= \y ->
           result (x,y)

-- | Returns the first item of the tuple
evalParse :: Parser a -> String -> a
evalParse p = (fst . head) . runParser p

-- returns the second item of the tuple
evalLog :: Parser a -> String -> String
evalLog p =  (snd . head) . runParser p

-- sat takes a predicate and yields a parse that consumes a single character if it satisfies or otherwise fails
sat' :: (Char -> Bool) -> Parser Char
sat' p = item >>= \x ->
         if p x then result x else zero

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then result x else zero

-- here are a bunch of predicates built by sat
-- char takes a char and an a string and if the first letter is the same char it gives result, otherwise it gives zeroupp
char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (`elem` ['0'..'9'])

lower :: Parser Char
lower = sat (`elem` ['a'..'z'])

upper :: Parser Char
upper = sat (`elem` ['A'..'Z'])

--firstTwoLower "tobi" ====> [("to","bi")]
firstTwoLower :: Parser String
firstTwoLower = lower >>= \x ->
                lower >>= \y ->
                result [x,y]


{- | Plus is a function that will help us combine parsers of arbitrary lengths to produce a final parser result
both argument parsers p and q are applied to the same input string and the result is concatenated to form 
a single result list
-}
plus :: Parser a -> Parser a -> Parser a
plus p q =  Parser $ \inp -> runParser p inp ++ runParser q inp

-- now we can use plus to combine our argument parsers: if both parsers succeed, they will both return their results
-- this reflects the ways input lists can be parsed in multiple ways
letter :: Parser Char
letter = lower `plus` upper

alphaNum :: Parser Char
alphaNum = letter `plus` digit

-- words will parse words (strings of letters) like so:
-- it will parse a non-empty word using a recusive call to word and the results are combined to form a string 
-- or nothing and the empty string
--              word "tobi" ====> [("tobi ",""),("tob ","i"),("to ","bi"),("t ","obi"),(" ","tobi")]
word :: Parser String
word = neWord `plus` result ""
        where neWord = letter >>= \x ->
                       word   >>= \xs ->
                       result (x:xs)

word2 :: Parser String
word2 = many1 letter

-- reads and recognises strings in the exact same order
--ghci> runParser (string "tobi is the") "tobi is the best at everything"    
--                        ====>     [("tobi is the"," best at everything")]
string :: String -> Parser String
string = traverse char


ogString :: String -> Parser String
ogString "" = result ""
ogString (x:xs) = char x >>= \fir ->
                  ogString xs >>= \rest ->
                  result (fir:rest)

--original many
ogMany :: Parser a -> Parser [a]
ogMany p = neParse `plus` pure []
        where neParse = do x <- p
                           xs <- ogMany p
                           result (x:xs)

-- many using force
many :: Parser a -> Parser [a]
many p = force $ neParse +++ pure []
        where neParse = do x <- p
                           xs <- many p
                           return (x:xs)

-- many1 is a special combinator that will only work on non-empty sequences: it is defined in terms of many
many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             result (x:xs)

-- ident will see if we have a lower case letter followed by any or many alpha numeric characters
ident :: Parser String
ident = do x <- lower
           xs <- many alphaNum
           result (x:xs)

-- using many1 we now have a parser for natural numbers:
-- ghci> runParser nat "1024" =====> [(1024,""),(102,"4"),(10,"24"),(1,"024")]
nat' :: Parser Int
nat' = do xs <- many1 digit
          return (eval xs)
        where
              eval xs = foldl1 op [ord x - ord '0' | x <- xs]
              op m n = 10*m + n

-- nat rewritten in the context of chainl1
nat :: Parser Int
nat = makeOrd `chainl1` return op
    where makeOrd = do x <- digit
                       return (ord x - ord '0')
          op m n = 10*m + n

int :: Parser Int
int = do f <- op
         n <- nat
         return $ f n
        where op = intParse `plus` return id
                where intParse = do _ <- char '-'
                                    return negate

ints' :: Parser [Int]
ints' = do _ <- char '['
           n <- int
           ns <- many1 $ do _ <- char ','
                            int
           _ <- char ']'
           return (n:ns)

-- sepby1 is a combinator like many 1 that recognises non-empty sequences of a parser but recognises that different instances
-- of a given parser p are separated by a parser sep whose result values are ignored
sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do x <- p
                    xs <- many1 $ do _ <- sep
                                     p
                    return (x:xs)

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = p `sepby1` sep `plus` return []


-- now ints can be given a new definition:
ints'' :: Parser [Int]
ints'' = do _ <- char '['
            ns <- int `sepby1` char ','
            _ <- char ']'
            return ns

-- bracketing of parsers by other parsers where the results are ignored is common enough to warrant its own combinator:
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do _ <- open
                          x <- p
                          _ <- close
                          return x
--some custom brackets:
sqbrack :: Parser a -> Parser a
sqbrack p = bracket (char '[') p (char ']')

pabrack :: Parser a -> Parser a
pabrack p = bracket (char '(') p (char ')')

-- now ints becomes:
ints :: Parser [Int]
ints = sqbrack (int `sepby1` char ',')

factor :: Parser Int
factor = nat `plus` pabrack expr

expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term =  factor `chainr1` expop

addop' :: Parser (Int -> Int -> Int)
addop' = add `plus` minus
    where add = do _ <- char '+'
                   return (+)
          minus = do _ <- char '-'
                     return (-)

-- addop rewritten in the context of ops
addop :: Parser (Int -> Int -> Int)
addop = ops [(char '+' , (+) ),
             (char '-' , (-) )
            ]

expop :: Parser (Int -> Int -> Int)
expop = ops [(char '^' , (^) )]

-- ops allow us to recursively put together operations
ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 plus op
    where op = do (p, op) <- xs
                  return $ do _ <- p
                              return op



-- chainl1 is a function that parses non-empty sequences of items separated by operators that associate to the left:
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
    where rest x = (op >>= \f ->
                    p  >>= \y ->
                    rest (f x y)) +++ return x

-- chainr1 pases a single p and then attempts to parse an operator and the rest of the sequence. If successful, the pair of
-- results from the first p and the rest of the sequence are combined using the function f. Otherwise, the sequence finishes
-- and p is returned.
chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op = p >>= \x ->
               (op >>= \f ->
                p `chainr1` op >>= \y ->
                return (f x y)) +++ return x

eval :: Parser Int
eval = add +++ sub
  where add = do x <- nat
                 _ <- char '+'
                 y <- nat
                 return (x+y)
        sub = do x <- nat
                 _ <- char '-'
                 y <- nat
                 return (x-y)

-- eval' is faster as this works in linear time -> it doesn't have to parse the whole string to figure out what to do
-- but is not necessary
eval' :: Parser Int
eval' = do x <- nat
           v <- add x +++ sub x
           return v
    where add x = do _ <- char '+'
                     y <- nat
                     return (x+y)
          sub x = do _ <- char '-'
                     y <- nat
                     return (x-y)

-- force makes it so that many p will always succeed, even if the p itself fails

force :: Parser a -> Parser a
force p = Parser $ \inp ->  let x = runParser p inp
                             in (fst (head x), snd (head x)) : tail x


number :: Parser Int
number = nat +++ result 0

--- only returns the first Parser to succed
first :: Parser a -> Parser a
first p = Parser $ \inp -> case runParser p inp of
                            []      -> []
                            (x:xs)  -> [x]

--- a function we can use using first to only get the first result back and improve efficiency
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p `plus` q)


--ghci> runParser colour "orange" =====> [("orange","")]
--ghci> runParser colour "yellow" =====>  [("yellow","")]
--ghci> runParser colour "blue"  ======> []
-- if we know that a Parser is deterministic, we can use (+++) for efficiency gains.
colour :: Parser String
colour =  p1 +++ p2
    where p1 = string "orange"
          p2 = string "yellow"

spaces :: Parser ()
spaces = do _ <- many1 (sat isSpace)
            return ()

comment :: Parser ()
comment = do _ <- string "--" +++ symbol "{-" +++ symbol "-}"
             _ <- many (sat (/= '\n'))
             return ()

-- with junk, parse and token, we can now have our parsers ignore whitespace.

junk :: Parser ()
junk = spaces +++ comment


parse :: Parser a -> Parser a
parse p = do _ <- junk
             p

token :: Parser a -> Parser a
token p = do p
             _ <- junk
             p

-- here are new examples of previous parsers that use the new parsers
natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier ks = token $ do x <- ident
                           let filt = const $ x `notElem` ks
                           return (filter filt x)

data Expr = App Expr Expr           -- application
          | Lam String Expr         -- lambda abstraction
          | Let String Expr Expr    -- local definition
          | Var String              -- variable
    deriving Show
pExpr = atom `chainl1` result App

atom =  lam +++ local +++ var +++ paren

lam = do _ <- symbol "\\"
         x <- variable
         _ <- symbol "->"
         Lam x <$> pExpr

local = do _ <- symbol "let"
           x <- variable
           _ <- symbol "="
           e <- pExpr
           _ <- symbol "in"
           Let x e <$> pExpr

var = do x <- variable
         return $ Var x


paren = bracket (symbol "(") pExpr (symbol ")")

variable :: Parser String
variable = identifier ["let", "in"]
