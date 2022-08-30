{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module ParserMIRIANA where
import GrammarMIRIANA
    ( BoolExpr(BooleanIdentifier, Or, And, Boolean, Not, LowerThan,
               GreaterThan, LowerEqualThan, GreaterEqualThan, EqualTo, Different),
      ArithExpr(ArithVariable, Sum, Sub, Mul, Div, Power, Sqrt, Constant,
                ArrVariable), ArrayExpr(ArrVariable, Array), Command (..) )


newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
  fmap g (P p) = P (\input -> case p input of
    Nothing -> Nothing
    Just (v, out) -> Just (g v, out) )


instance Applicative Parser where
  pure v = P (\input -> Just (v, input))

  (P pg) <*> px = P (\input -> case pg input of
    Nothing -> Nothing
    --[(g, out)] -> case fmap g px of (P p) -> p out)
    Just (g, out) -> case fmap g px of (P p) -> p out )


instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (P p) >>= f = P (\input -> case p input of
                Nothing -> Nothing
                Just (v, out) -> case f v of (P q) -> q out)   -- apply f to the result v to give another parser f v 


class Monad f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  many :: f a -> f [a]
  some :: f a -> f [a]
  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x
  chain :: f a -> f (a -> a -> a) -> f a
  chain p op = do a <- p; rest a
    where
        rest a = (do f <- op; a' <- p; rest (f a a')) <|> return a


instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\input -> Nothing)
  -- <|> :: Parser a -> Parser a -> Parser b
  (P p) <|> (P q) = P (\input -> case p input of
                                  Nothing -> q input
                                  Just (v, out) -> Just (v, out))

-- Reads the next character from the string given as input
readNext :: Parser Char
readNext = P (\input -> case input of
                      [] -> Nothing
                      (x:xs) -> Just (x,xs))

-- Checks if x satisfies a property given as input
sat :: (Char -> Bool) -> Parser Char
sat p =
  do {
    x <- readNext;
    if p x then return x else empty;
  }


--Using sat and appropriate predicates from the library Data.Char, we can now define parsers for single digits, 
--lower-case letters, upper-case letters, arbitrary letters, alphanumeric characters, and specific characters

--DIGIT
digitCase :: Parser Char
digitCase = sat isDigit

digit :: [Char]
digit = ['0' .. '9']

isDigit :: Char -> Bool
isDigit x = elem x digit

--LOWER CASE
lowerCase :: Parser Char
lowerCase = sat isLower

lowers :: [Char]
lowers = ['a' .. 'z']

isLower :: Char -> Bool
isLower x = elem x lowers

--UPPER CASE
upperParser :: Parser Char
upperParser = sat isUpper

uppers :: [Char]
uppers = ['A' .. 'Z']

isUpper :: Char -> Bool
isUpper x = elem x uppers

--LETTER
isLetter :: Char -> Bool
isLetter x = isUpper x || isLower x

letter :: Parser Char
letter = sat isLetter

--ALPHANUM
isAlphaNum :: Char -> Bool
isAlphaNum x = isLetter x || isDigit x

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

-- Single char
char :: Char -> Parser Char
char x = sat (== x)

-- using char we can define a parser string xs for the string of characters xs, with the string
-- itself returned as the result value
string :: String -> Parser String
string [] = return []
string (x : xs) =
    do
        char x
        string xs
        return (x : xs)

--Using many and some, we can now define parsers for identifiers (variable names) comprising a lowercase
--letter followed by zero or more alphanumeric characters, natural numbers comprising one or more
--digits, and spacing comprising zero or more space, tab, and newline characters:
anIdentifier :: Parser String
anIdentifier =
    do
        x <- letter
        xs <- many alphaNum    -- Many takes from 0 to n
        return (x : xs)


-- Natural number with Integer
nat :: Parser Int
nat =
    do
        xs <- some digitCase
        return (read xs)

--Integer positive and negative numbers
int :: Parser Int
int =
    do
    char '-'
    n <- nat
    return (-n)
    <|> nat

-- Natural number with Float
natFloat :: Parser Float
natFloat =
    do float
    where float = do 
                    xs <- some digitCase 
                    symbol "e" 
                    symbol "+" 
                    xs <- some digitCase
                    return (read xs)
                  <|> 
                  do 
                    xs <- some digitCase 
                    symbol "e" 
                    symbol "-" 
                    xs <- some digitCase
                    return (read xs)
                  <|> 
                  do 
                    xs <- some digitCase 
                    symbol "." 
                    xs <- some digitCase
                    return (read xs)
                  <|> 
                  do 
                    xs <- some digitCase 
                    symbol "." 
                    xs <- some digitCase
                    symbol "e" 
                    symbol "-" 
                    xs <- some digitCase
                    return (read xs)
       


--Float positive and negative numbers
float :: Parser Float
float =
    do
    char '-'
    nf <- natFloat
    return (-nf)
    <|> natFloat


--Spaces
spaces :: [Char]
spaces = ['\n', '\t', '\r', ' ']

isSpace :: Char -> Bool
isSpace x = elem x spaces

aSpace :: Parser ()
aSpace = do
            many (sat isSpace)
            return ()

--Most real-life parsers allow spacing to be freely used around the basic tokens in their input string. For
--example, the strings 1+2 and 1 + 2 are both parsed in the same way by GHC. To handle such spacing, we
--define a new primitive that ignores any space before and after applying a parser for a token
token :: Parser a -> Parser a         -- deletes spaces
token p =
    do
        aSpace
        v <- p
        aSpace
        return v

-- Using token, we can now define parsers that ignore spacing around identifiers, natural numbers,
--integers and special symbols
identifier :: Parser String
identifier = token anIdentifier

naturalNumber :: Parser Int
naturalNumber = token int

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- ARITHMETIC EVALUATION --

-- Sequencing in the grammar is translated into the do notation, choice | is translated into the <|> operator, 
--the empty string ϵ becomes the empty parser, special symbols such as + and * are handled using the symbol function, 
--and natural numbers are parsed using the natural primitive

arithExp  :: Parser ArithExpr
arithExp = do chain arithTerm op
    where  op =  (do symbol "+"; return Sum)
                 <|> do symbol "-"; return Sub

arithTerm :: Parser ArithExpr
arithTerm = do chain arithFactor op
    where op = (do symbol "*"; return Mul)
            <|> (do symbol "/"; return Div)
            <|> (do symbol "^"; return Power)
            <|> do symbol "!^"; return Sqrt

arithFactor :: Parser ArithExpr
arithFactor = do
    (Constant <$> integer)
        <|> do
            i <- identifier
            do
                symbol "["
                n <- arithExp
                symbol "]"
                return (ArrayVariable i n)
                <|> return (ArithVariable i)
        <|> do
            symbol "("
            a <- arithExp
            symbol ")"
            return a

--Boolean
boolExp :: Parser BoolExpr
boolExp = chain boolTerm op
  where op = do
            symbol "Or"
            return Or

boolTerm :: Parser BoolExpr
boolTerm = chain boolFact op
  where op = do
            symbol "And"
            return And

--
boolFact :: Parser BoolExpr
boolFact =
  do
    symbol "True"
    return (Boolean True)
    <|> do
      symbol "False"
      return (Boolean False)
    <|> do
      symbol "Not"
      Not <$> boolExp
    <|> do
      symbol "("
      b <- boolExp
      symbol ")"
      return b
    <|> do
        a1 <- arithExp
        do
            symbol "<"
            a2 <- arithExp
            return (LowerThan a1 a2)
            <|> do
              symbol ">"
              a2 <- arithExp
              return (GreaterThan a1 a2)
            <|> do
              symbol "<="
              a2 <- arithExp
              return (LowerEqualThan a1 a2)
            <|> do
              symbol ">="
              a2 <- arithExp
              return (GreaterEqualThan a1 a2)
            <|> do
              symbol "=="
              a2 <- arithExp
              return (EqualTo a1 a2)
            <|> do
              symbol "!="
              a2 <- arithExp
              return (Different a1 a2)
           <|> (BooleanIdentifier <$> identifier)


--Commansds
command :: Parser Command
command =
   ifThenElse 
   <|> while 
   <|> arithDeclare 
   <|> boolDeclare 
   <|> arrayDeclare 
   <|> arithAssign 
   <|> boolAssign 
   <|> arrOneAssign 
   <|> arrMulAssign 
   <|> skip


program :: Parser [Command]         
program = do many command                   


arithDeclare :: Parser Command
arithDeclare =
  do
    symbol "int"
    id <- identifier
    do
        symbol "="
        value <- arithExpr
        symbol ";"
        return (ArithDeclare id (Just value))
        <|>
        do
            symbol ";"
            return (ArithDeclare id Nothing)


boolDeclare :: Parser Command
boolDeclare =
  do
    symbol "bool"           
    id <- identifier
    symbol "="
    val <- boolExpr
    symbol ";"
    return (BoolDeclare id (Just val))
    <|>
    do
      symbolP ";"
      return (BoolDeclare id Nothing)


arrDeclare  :: Parser Command
arrDeclare  =
  do
    symbol "array"            
    symbol "["
    size <- arithExpr
    symbolP "]"
    name <- identifier
    do
        symbol "="
        value <- arithExpr
        symbol ";"
        return (ArrDeclare name size (Just value))
        <|>
        do
            symbol ";"
            return (ArrDeclare name size Nothing)

arithAssign :: Parser Command
arithAssign =
  do
    i <- identifier
    symbol "="
    value <- arithExpr
    symbol ";"
    return (ArithAssign id value)
    

boolAssign  :: Parser Command
boolAssign  =
  do
    id <- identifier
    symbol "="
    value <- boolExpr
    symbol ";"
    return (BoolAssign id value)


arrOneAssign :: Parser Command
arrOneAssign = do
    name <- identifier
    symbol "["
    position <- arithExpr
    symbol "]"
    symbol "="
    value <- arithExpr
    symbol ";"
    return (ArrOneAssign name position value)



arrMulAssign  :: Parser Command
arrMulAssign  =
  do
    i <- identifier  
    do 
    symbol "="
    symbol "["
    a <- arithExp
    b <- many (do symbol ","; arithExp)
    symbol "]"
    symbol ";"
    return (ArrMulAssign i (Array (a:b)))
    <|>
      do 
        symbol "["
        symbol "]"
        symbol "="
        x <- identifier
        symbol "["
        symbol "]"
        symbol ";"
        return (ArrMulAssign i (ArrVariable x)) 

skip  :: Parser Command
skip  =
  do
    symbol "skip"
    symbol ";"
    return Skip 


ifThenElse :: Parser Command
ifThenElse =
  do
    symbol "if"
    symbol "("
    b <- boolExp
    symbol ")"
    symbol "{"
    thenP <- program
    symbol "}"
    do
      symbol "else"
      symbol "{"
      elseP <- program
      symbol "}"
      return (Ifelse b thenP (Just elseProg))
      <|> do
        return (Ifelse b thenP Nothing)


while :: Parser Command
while =
  do
    symbol "while"
    symbol "("
    b <- boolExp
    symbol ")"
    symbol "{"
    p <- program
    symbol "}"
    return (While b p)

