
module Main where
--import System.Console.Haskeline
--import System.Exit
import System.Environment ( getArgs )

import Repl
import Errors
import Types


context = initCtx

main :: IO ()
main =  getArgs >>= \argv -> 
     if' (length argv == 0) (repl context) (manager argv context (elem "-i" argv))
