
module Main where
import System.Environment ( getArgs )

import Repl
import Errors
import Types
import SchEval

funcF:: SchVal
funcF = SchString "HAHAHA!"

context :: Ctx
context = [("+", Closure ["x"] (Plus [Var "x"]) context), ("name", (SchString "Mama")), ("Three", (SchFloat 3.0)), ("fun", funcF)]

main :: IO ()
main = getArgs >>= \argv -> 
     if' (length argv == 0) (repl context) (manager argv context (elem "-i" argv))
