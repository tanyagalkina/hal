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


-- env = []


-- zeroto :: Int -> [Int]
-- zeroto n = [0..n]

-- cannotReadFile :: [String] -> Bool
-- cannotReadFile _ = False


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

direct_env_two :: SchVal -> Ctx ->(Int, SchVal, Ctx)
direct_env_two (Env i) old = (0, SchQuote (fst $ head i), i)
direct_env_two n      old  = (0, n, old)


schedule :: [SchExpr] -> Ctx -> (Int, SchVal, Ctx)
schedule (x:[]) ctx = direct_env (eval (x) ctx) ctx
schedule (x:xs) ctx = schedule xs newctx
  -- direct_env (eval (x) ctx) ctx
                      where (a, b, newctx) =  direct_env ( eval x ctx) ctx


evalSchTwo :: String -> Ctx -> (Int, SchVal, Ctx)
evalSchTwo input  ctx | output == [] =  (84, (SchString "something went wrong"), ctx)
                   | snd (head output) /= "" = (84, (SchString "Did Not Parse Everything"), ctx)
                   -- True = (direct_env (eval (head (fst $ head output)) ctx) ctx)
                   | True = schedule (fst $ head output) ctx
                        -- where output = parse scheme input
                          where output =  parse atoms input


-- manageInputFeed :: String -> [String] -> Ctx -> Bool -> IO ()
-- manageInputFeed input as ctx flag = 




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
                -- manageInputFeed s as ctx flag
                if' (code == 84) (b (sP res) 84) (if' (as == []) (pSL (sP res) >> m as ct flag) (m as ct flag))
                where (code, res, ct) = evalSchTwo (stripChars "\t\n\r" $ s) ctx
