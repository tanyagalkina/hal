module Grammer where

import Lexer
import Types
import Control.Applicative

--FUNC RETURN ABSTRACT EXPRESSION
scheme :: Parser SchExpr
scheme = do
        n <- tokenFlt
        k <- tokenFlt
        j <- tokenFlt
        u <- tokenFlt
        --return (Plus (map typing ([n]:[k]:[j])))
        return (Minus [n, k, j, u])
        --return (Minus [(Float n), (Float k)])
        <|> return (Err "Hello!")

-- fromPairs = do
--         _ <- string "'" <|> string ""
--         n <- list

--         return ("(" ++ n ++ ")")    
--         <|> return "fromPair did not get a list back" 

-- list :: Parser String
-- list = do
--       _ <- string "("

--       elem1 <- element
--       elem2  <- list
--       br2 <- string ")"

--       if elem2  == []
--           then return elem1
--       else
--          return (elem1 ++ elem2)
--       <|> element
--       -- <|> element

-- element:: Parser String
-- element = do
--       x <- token <|> dottedPair
--       _  <- dot
--       y <- list
--       if y == []
--           then return x
--       else    
--         return (x ++ " " ++ y)
--       <|> dottedPair


-- dottedPair :: Parser String
-- dottedPair = do
--         brace <- string "("


--         x <- token
--         --return (brace ++ x)
--         _ <- dot

--         y <- token
--         _ <- string ")"

--         return ("(" ++ x ++ " . " ++ y ++ ")")
--         <|> return []

nil :: Parser String
nil = do
        _ <- string "("
        _ <- string ")"
        return "nil"
