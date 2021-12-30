module Errors
where

import Lexer
import System.Console.Haskeline
import System.Exit
import Repl
import Types
import SchEval
import Control.Exception
import Debug.Trace

braveExit :: String -> Int -> IO ()
braveExit str 0 = putStrLn str >> exitWith (ExitSuccess)
braveExit str n = putStrLn str >> exitWith (ExitFailure n)


env = []


zeroto :: Int -> [Int]
zeroto n = [0..n]

cannotReadFile :: [String] -> Bool
cannotReadFile _ = False


-- mPrint :: [String] -> String -> Ctx -> IO ()
-- mPrint as ctx = if' (code == 84) (b (sP res) 84) (if' (as == []) (pSL (sP res) >> m as ct flag) (m as ct flag))
--                 where (code, res, ct) = evalSch s ctx


-- stripChars " \t" 

-- stripChars :: String -> String -> String
-- stripChars = filter . flip notElem


b = braveExit
sP = schPrint
pSL = putStrLn
m = manager


--- ---------[ARGS]   -> CTX
manager :: [String] -> Ctx -> Bool -> IO ()
manager ("-i":as) ctx flag = manager as ctx flag
manager [] ctx True = repl ctx
manager [] ctx False = exitWith ExitSuccess
manager (a:as) ctx flag =
        do
          src <- try $ readFile a
          case src of
              Left e ->  print (e :: IOError)
                          >> exitWith (ExitFailure 84)

    --   as == [] MEANS IT IS LAST FILE NOW HAVE TO FIX AND CHANGE IT TO LAST EXPRESSION         
              Right s -> 
                if' (code == 84) (b (sP res) 84) (if' (as == []) (pSL (sP res) >> m as ct flag) (m as ct flag))
                where (code, res, ct) = evalSch (stripChars "\t\n\r" $ s) ctx
