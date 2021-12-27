module Errors
( 
  manager
  ,braveExit
  ,zeroto
  , if'
  
) where

import Lexer
import System.Console.Haskeline
import System.Exit
import Repl
import Types
import SchEval
import Control.Exception
--import System.Environment (getArgs)


--else braveExit "share something if u dont mind ..." 84  //EXAMPLE
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
              --- IF THERE WERE PROBLEMS WITH READING THE FILE
              Left e ->  print (e :: IOError)
                          >> exitWith (ExitFailure 84)
              -- GOT THE FILE CONTENT, THEN ...->

              Right s -> 
                if' (code == 84) (b (sP res) 84) (if' (as == []) (pSL (sP res) >> m as ct flag) (m as ct flag))
                  where (code, res, ct) = evalSch s ctx
              -- Right s -> do 
              --   -- let r@(code, res, ct) =  evalLisp s ctx
              --   let r@(code, res, ct) = evalSch s ctx

              --   if' (code == 84) (braveExit (schPrint res) 84) (if' (as == []) (putStrLn (schPrint res) >> manager as ct flag) (manager as ct flag))      
