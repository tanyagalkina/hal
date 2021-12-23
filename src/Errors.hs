module Errors
( 
  manager
  ,braveExit
  ,zeroto
  , if'
  
) where

import System.Console.Haskeline
import System.Exit
import Repl
import Types
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


if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


--- ---------[ARGS]   -> CTX
manager :: [String] -> Ctx -> Bool -> IO ()
manager [] ctx True = repl ctx
manager [] ctx False = exitWith ExitSuccess
manager (a:as) ctx flag =
        do
          src <- try $ readFile a
          case src of
              --- IF THERE WERE PROBLEMS WITH READING THE FILE
              Left e -> do
                          print (e :: IOError)
                          >> exitWith (ExitFailure 84)
              -- GOT THE FILE CONTENT, THEN ...->
              Right s -> do 
                let r@(code, res, ct) =  evalLisp s ctx
                if' (code == 84) (braveExit res 84) (if' (as == []) (print res >> manager as ct flag) (manager as ct flag))      
