module Lexer
where


import Types
import System.Exit
import System.Environment (getArgs)
import Data.Char
import Control.Applicative
import Debug.Trace
import Data.Typeable
import Data.Kind

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= \x ->
           if p x then return x else empty


isFloatDigit          :: Char -> Bool
isFloatDigit c        = (c == '.' ||
       (fromIntegral (ord c - ord '0') :: Word) <= 9)


notBrace :: Char -> Bool
notBrace c = (c /= ')')

notQuote :: Char -> Bool
notQuote c = (c /= '"')


digitF' :: Parser Char
digitF' = sat isFloatDigit

char :: Char -> Parser Char
char x = sat (== x)


validFloat :: String -> Bool
validFloat xs | len == 1 || len == 0 = True
               | otherwise = False
                  where len =  (length $ filter (== '.') xs)


posflt :: Parser Double
posflt = do
            -- _ <- many spaces 
            char '+'
            n <- pflt
            return (n)
            <|> pflt


pflt :: Parser Double
pflt =  do
        -- _ <- many spaces
        xs <- some digitF'
        --some digitF' >>= \xs ->
        if validFloat xs
           then return (read xs)
           else empty

flt :: Parser Double
flt = do 
         char '-'
         n <- pflt
         return (-n)
         <|> posflt


stringFloat :: Parser String
stringFloat =  do 
        x <- some alphanum
        return x


alphanum :: Parser Char
alphanum = sat isAlphaNum

sch :: Parser Char
sch = sat isSchemeSymbol <|> sat isAlphaNum

dat :: Parser Char
dat = sat isAlphaNum 

daten :: Parser String
daten = do
         d <- many dat
         return d


isSchemeSymbol :: Char -> Bool
isSchemeSymbol ch = elem ch "!#$%&|*+-/:<=>?@^_~"

isMathSymbol :: Char -> Bool
isMathSymbol ch = elem ch "+-*/<"

tokenFlt :: Parser SchExpr
tokenFlt = do
            _ <- spaces
            f <- flt
            return (Float f)
            <|> empty



token :: Parser SchExpr
token = do
            _ <- spaces
            t <- some alphanum
            return (Var t)

var :: Parser String
var = do
            _ <- spaces
            t <- some alphanum
            return (t)            
            
eq:: Parser SchExpr
eq = do
      _ <- char 'e'
      _ <- char 'q'
      _ <- char '?'
      return (Var "eq?")

atm:: Parser SchExpr
atm = do
      at <- string "atom"
      qq <- char '?'
      return (Var "atom?")

boolean :: Parser SchExpr
boolean = do
            b <- string "#t" <|> string "#f"
            if' (b == "#t") (return (Bool True)) (return (Bool False))                                

--NOT VALID FOR STRINGS WHICH START WITH SPACES! PLEASE SPLIT!
string :: String -> Parser String
string []     = return []
string s@(x:xs) = do
                       
                            _  <- many (sat isSpace)
                            _   <- char x
                            _ <- string xs
                            return (x:xs)


symb:: Parser SchExpr
symb = do
        s <- (sat isMathSymbol) 
        return (Var [s])



letter :: Parser Char
letter = sat isAlphaNum <|> sat isSchemeSymbol

quoteSymb :: Parser SchExpr
quoteSymb = do
        _ <- spaces 
        s <- some letter
        -- (sat isSchemeSymbol) <|> (sat isAlphaNum)
        return (QSymb s)



spaces :: Parser String
spaces = do 
           _ <- many (sat isSpace)
           return ""

 
atleastoneSpace :: Parser Char
atleastoneSpace = do
        _  <- some (sat isSpace)
        return ' '
        <|> empty

dot :: Parser String
dot = do
        _ <-atleastoneSpace
        --return " . " 
        _ <- char '.' 
        _ <- char ' '
        return " . "
        <|> empty


checkString :: Parser String
checkString = do
        _ <- string "Chorowo!"
        return "Chorowo!"
        <|> return []


parseSchExpr :: Parser SchVal
parseSchExpr = do
              _ <- string ""
              return $ AtmString "STRING"  


quotedString :: Parser SchExpr
quotedString = do
             _ <- spaces
             _ <- char '"'
             quot <- daten
             _ <- char '"' 
             return (Str quot)
             