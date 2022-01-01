
module Main where
import System.Environment ( getArgs )

import Repl
import Errors
import Types
import SchEval


context :: Ctx
context = [
            ("div", Closure ["!a", "!b"] (Div [Var "!a", Var "!b"]) context)
           ,("mod", Closure ["!c", "!d"] (Mod [Var "!c", Var "!d"]) context)
           ,("<", Closure ["!e", "!f"] (Less [Var "!e", Var "!f"]) context)
           ,("+", Closure ["!g", "!h"] (Plus [Var "!g", Var "!h"]) context)
           ,("-", Closure ["!i", "!j"] (Minus [Var "!i", Var "!j"]) context)
           ,("*", Closure ["!k", "!l"] (Mult [Var "!k", Var "!l"]) context)
           ,("eq?", Closure ["!m", "!n"] (Equals [Var "!m", Var "!n"]) context)
           ,("atom?", Closure ["!o"] (IsAtom [Var "!o"]) context)
           ,("cons", Closure ["!p", "!q"] (Cons [Var "!p", Var "!q"]) context)
           ,("car", Closure ["!r"] (Car [Var "!r"]) context)
           ,("cdr", Closure ["!t"] (Cdr [Var "!t"]) context)
           ,("cond", Closure ["!u", "!v", "!w", "!x", "!y", "!z"] 
                    (Cond [Var "!u", Var "!v", Var "!w", Var "!x", Var "!y", Var "!z"]) context)
          ]

main :: IO ()
main = getArgs >>= \argv -> 
     if' (length argv == 0) (repl context) (manager argv context (elem "-i" argv))
