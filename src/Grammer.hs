module Grammer where

import Lexer
import Types
import Control.Applicative

atoms :: Parser [SchExpr]
atoms = do
        atoms <- some atom
        return atoms

atom :: Parser SchExpr
atom = do
                n <- tokenFlt
                return n
                <|> boolean
                <|> quote
                <|> quotedString
                <|> eq
                <|> atm
                <|> token
                <|> symb
                <|> lambda
                <|> define
                <|> lett
                <|> cond
                <|> list

quoteAtom :: Parser SchExpr
quoteAtom = do
        _ <- spaces 
        n <- tokenFlt
        return n
        <|> quotedString
        <|> quoteSymb


defn :: Parser Defn
defn = do
        _ <- spaces
        name <- some letter
        val <- atom
        return (SchVal name val)


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
        name <- some letter
        _ <- spaces
        ins  <-  some var
        _ <- string ")" 
        ex <- list
        _ <- string ")"
        return (Def (Rec name (Lam ins ex))) 
        <|> defineVal


lambda :: Parser SchExpr
lambda = do
        _ <- string "("
        _ <- string "lambda"
        _ <- spaces
        _ <- string "("
        _ <- spaces
        ins  <-  some var
        _ <- string ")" 
        ex <- list
        _ <- string ")"

        return (Lam ins ex)


lett :: Parser SchExpr
lett = do 
        _ <- string "("
        _ <- string "let"
        _ <- string "("  
        ins  <- some list
        _ <- string ")"
        ex <- list
        _ <- string ")"
        return (Lett ins ex) 


cond :: Parser SchExpr
cond = do
        _ <- string "("
        _ <- string "cond"
        body <- some list
        _ <- string ")"
        return (Cond body)


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
listMan []   = Err "Invalid Sytax"
listMan list = (Li list)             


processFirst :: [SchExpr] -> [SchExpr]
processFirst (x:xs) = (Head x):xs


concat_qlist :: [String]-> String -> String
concat_qlist (x:[]) base = x ++ " " ++ base
concat_qlist (x:xs) base | base == ")" = (concat_qlist xs ((x) ++ base))
                         | otherwise = (concat_qlist xs ((x) ++ " " ++ base))       


quoted_list :: Parser [SchExpr]
quoted_list = do
             _ <- string "("
             q <- many quoteAtom 
             _<- string ")"
             return q

symbol_quote :: Parser SchExpr
symbol_quote = do
               _ <- string "("
               _ <- string "quote"
               _ <- spaces
               t <- quoteAtom
               _ <- string ")"
               return (Quote t) 
              -- <|> symbol_QList

symbol_QList :: Parser SchExpr
symbol_QList = do 
        _ <- string "("
        _ <- string "quote"
        _ <- spaces
        t <- many quoteAtom
        _ <- string ")"
        return (QList t)


qList :: Parser SchExpr
qList = do
        _ <- string "'"
        t <- quoted_list 
        return (QList t)

qPatternList :: Parser SchExpr
qPatternList = do
        _ <- string "("
        _ <- string "quote"
        _ <- spaces
        t <- quoted_list 
        _ <- string ")"
        return (QList t)


qPattern :: Parser SchExpr
qPattern = do
        _ <- string "("
        _ <- string "quote"
        _ <- spaces
        t <- quoteAtom
        _ <- string ")"
        return (Quote t)

quote :: Parser SchExpr
quote = do
        _ <- string "'"
        _ <- spaces
        t <- quoteAtom
        return (Quote t)
        <|> qPattern 
        <|> qPatternList
        <|> qList

-- quotes :: Parser String
-- quotes = do
--             _ <- spaces
--             t <- some sch
--             return (t)

