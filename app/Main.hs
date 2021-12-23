
module Main where
--import System.Console.Haskeline
--import System.Exit
import System.Environment ( getArgs )

import Repl
import Errors

ctx = "Mama!"

main :: IO ()
main =  getArgs >>= \argv -> 
     if' (length argv == 0) (repl ctx) (manager argv ctx (elem "-i" argv))
