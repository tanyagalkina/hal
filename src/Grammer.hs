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
                <|> list

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
