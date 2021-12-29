
module Main where
import System.Environment ( getArgs )

import Repl
import Errors
import Types
import SchEval

funcF:: SchVal
funcF = SchString "HAHAHA!"

context :: Ctx
context = [
            ("div", Closure ["x", "y"] (Div [Var "x", Var "y"]) context)
           ,("mod", Closure ["x", "y"] (Mod [Var "x", Var "y"]) context)
           ,("<", Closure ["x", "y"] (Less [Var "x", Var "y"]) context)
           ,("+", Closure ["x", "y"] (Plus [Var "x", Var "y"]) context)
           ,("-", Closure ["x", "y"] (Minus [Var "x", Var "y"]) context)
           ,("*", Closure ["x", "y"] (Mult [Var "x", Var "y"]) context)
           ,("eq?", Closure ["x", "y"] (Equals [Var "x", Var "y"]) context)
           ,("cons", Closure ["x", "y"] (Cons [Var "x", Var "y"]) context)
           ,("car", Closure ["x"] (Car [Var "x"]) context)
           ,("cdr", Closure ["x"] (Cdr [Var "x"]) context)
           ,("name", (SchString "Mama"))
           ,("Three", (SchFloat 3.0))
           ,("fun", funcF)
          ]

main :: IO ()
main = getArgs >>= \argv -> 
     if' (length argv == 0) (repl context) (manager argv context (elem "-i" argv))
