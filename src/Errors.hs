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
import Grammer

braveExit :: String -> Int -> IO ()
braveExit str 0 = putStrLn str >> exitWith (ExitSuccess)
braveExit str n = putStrLn str >> exitWith (ExitFailure n)

b :: String -> Int -> IO ()
b = braveExit

sP :: SchVal -> String
sP = schPrint

pSL :: String -> IO ()
pSL = putStrLn

m :: [String] -> Ctx -> Bool -> IO () 
m = manager

direct_env_two :: SchVal -> Ctx ->(Int, SchVal, Ctx)
direct_env_two (Env i) old = (0, SchQuote (fst $ head i), i)
direct_env_two (Error i) old = (84, (SchQuote i), old)
direct_env_two n      old  = (0, n, old)


schedule :: [SchExpr] -> Ctx -> (Int, SchVal, Ctx)
schedule (x:[]) ctx = direct_env (eval (x) ctx) ctx
schedule (x:xs) ctx | code == 84 = (84, res, newctx)
                    | otherwise = schedule xs newctx
                      where (code, res, newctx) =  direct_env_two ( eval x ctx) ctx


evalSchTwo :: String -> Ctx -> (Int, SchVal, Ctx)
evalSchTwo input  ctx | output == [] =  (84, (SchString "something went wrong"), ctx)
                   | snd (head output) /= "" = (84, (SchString "Did Not Parse Everything"), ctx)
                   -- True = (direct_env (eval (head (fst $ head output)) ctx) ctx)
                   | True = schedule (fst $ head output) ctx
                        -- where output = parse scheme input
                          where output =  parse atoms input


manageInputFeed :: String -> [String] -> Ctx -> Bool -> IO ()
manageInputFeed input as ctx flag
          | code == 84 = ( b ( sP res) 84)
          | otherwise = (if' (as == []) 
              (pSL (sP res) >> m as ct flag) (m as ct flag))
      where ( code, res, ct) = evalSchTwo (stripChars "\t\n\r" $ input) ctx


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
              Right s -> 
                manageInputFeed s as ctx flag
