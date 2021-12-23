module Lexer (
fromPairs
,Parser
,parse
)
where


import System.Exit
import System.Environment (getArgs)
import Data.Char
import Control.Applicative
import Debug.Trace


newtype Parser a = P (String ->[(a, String)])

parse :: Parser a -> String -> [(a,String)]
--parse (P p) input = traceShow ("trace" ++ show input) p input
parse (P p) input = p input


item :: Parser Char
item = P (\input -> case input of
                     []     -> []
                     (x:xs) -> [(x,xs)])


instance Functor Parser where
   -- fmap :: (a -> b) -> Parser a -> Parser b
   fmap g p = P (\input -> case parse p input of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   -- pure :: a -> Parser a
   pure v = P (\input -> [(v,input)])

   -- <*> :: Parser (a -> b) -> Parser a -> Parser b
   pg <*> px = P (\input -> case parse pg input of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
   p >>= f = P (\input -> case parse p input of
                           []        -> []
                           [(v,out)] -> parse (f v) out)


instance Alternative Parser where
   -- empty :: Parser a
   empty = P (\input -> [])

   -- (<|>) :: Parser a -> Parser a -> Parser a
   p <|> q = P (\input -> case parse p input of
                           []        -> parse q input
                           [(v,out)] -> [(v,out)])


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


isSecondChoice          :: Char -> Bool
isSecondChoice c      = (c == '-' || c == '+')
       


validFloat :: String -> Bool
validFloat xs | len == 1 || len == 0 = True
               | otherwise = False
                  where len =  (length $ filter (== '.') xs)



secondChoiceOp :: Parser Char
secondChoiceOp = sat isSecondChoice


multFloat:: Parser Float
multFloat = do
               char '*'
               n <- pflt
               return (n)
            

posflt :: Parser Float
posflt = do char '+'
            n <- pflt
            return (n)
            <|> pflt



pflt :: Parser Float
pflt =  some digitF' >>= \xs ->
              if validFloat xs
                  then return (read xs)
              else empty



binFloat:: Parser Float
binFloat = do
               char '/'
               n <- pflt
               return (1/n)
               <|> multFloat

-------------------------------
flt :: Parser Float
flt = do char '-'
         n <- pflt
         return (-n)
         <|> posflt


stringFloat :: Parser String
stringFloat =  do 
        x <- some alphanum
        return x


braveExit :: String -> Int -> IO ()
braveExit str 0 = putStrLn str >> exitWith (ExitSuccess)
braveExit str n = putStrLn str >> exitWith (ExitFailure n)


stripChars :: String -> String -> String
stripChars = filter . flip notElem



alphanum :: Parser Char
alphanum = sat isAlphaNum


dat :: Parser Char
dat = sat isAlphaNum

daten :: Parser String
daten = do
         d <- many dat
         return d


token :: Parser String
token = do
            t <- many alphanum
            return t


string :: String -> Parser String
string []     = return []
string (x:xs) = do 
                   _ <- char x
                   _ <- string xs
                   return (x:xs)


space :: Parser ()
space = do many (sat isSpace)
           return ()

tocken :: Parser a -> Parser a
tocken p = do
             space
             v <- p
             space
             return v
 
fromPairs:: Parser String
fromPairs = do
        _ <- string "'" <|> string ""

        n <- list

        return ("(" ++ n ++ ")")    
        <|> return "fromPair did not get a list back" 

list :: Parser String
list = do
      _ <- string "("

      elem1 <- element
      elem2  <- list
      br2 <- string ")"

      if elem2  == []
          then return elem1
      else
         return (elem1 ++ elem2)
      <|> element


element:: Parser String
element = do
      x <- some alphanum <|> pair
      str <- string " . "
      y <- list
      if y == []
          then return (x)
      else    
        return (x ++ "  " ++ y)
      <|> pair


nil :: Parser String
nil = do
        _ <- string "("
        _ <- string ")"
        return "nil"

pair :: Parser String
pair = do
        _ <- string "("
        x <- some alphanum
        _ <- string " . "
        y <- some alphanum
        _ <- string ")"
        return ("(" ++ x ++ " . " ++ y ++ ")")
        <|> return []

        
quotedString :: Parser String
quotedString = do
             _ <- char '"'
             quot <- daten
             _ <- char '"' 
             return quot
             <|> empty


isAtom :: Parser String
isAtom = do
         x <- quotedString
         return x
         <|> return "tanya"
