module Grammer where

import Lexer
import Types
import Control.Applicative

atom :: Parser SchExpr
atom = do
                n <- tokenFlt
                return n
                <|> quotedString
                <|> eq
                <|> atm
                <|> token
                <|> symb
                <|> boolean
                <|> quote
                <|> define
                <|> list

defn :: Parser Defn
defn = do
        _ <- spaces
        nam <- some alphanum
        val <- atom
        return (SchVal nam val)


defineVal :: Parser SchExpr
defineVal = do
        _ <- string "("
        _ <- string "define"
        _ <- spaces
        d <- defn
        _ <- string ")"
        return (Def d)

define :: Parser SchExpr
define = do
        _ <- string "("
        _ <- string "define"
        _ <- spaces
        _ <- string "("
        _ <- spaces 
        name <- some alphanum
        _ <- spaces
        ins  <-  some var
        _ <- string ")" 
        ex <- list
        _ <- string ")"
        return (Def (Rec name (Lam ins ex))) 
        <|> defineVal


list :: Parser SchExpr
list = do
       _ <- string "("
       _ <- spaces
       ex <- many atom
       _ <- string ")"
       return (listMan ex)

listMan :: [SchExpr] -> SchExpr
listMan (Var "+": xs) = (Plus xs)
listMan (Var "-":xs) = (Minus xs)
listMan (Var "*": xs) = (Mult xs)
-- listMan (Var "cons":xs) = (Li $ processFirst xs)
listMan []   = Err "Invalid Sytax"
listMan list = (Li list)             


processFirst :: [SchExpr] -> [SchExpr]
processFirst (x:xs) = (Head x):xs
