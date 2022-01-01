module Repl
where

import Debug.Trace
import System.Console.Haskeline
import System.Exit
import Types
import SchEval
import Grammer

direct_env :: SchVal -> Ctx ->(Int, SchVal, Ctx)
direct_env (Env i) old = (0, SchQuote (fst $ head i), i)
direct_env n      old  = (0, n, old)

evalSch :: String -> Ctx -> (Int, SchVal, Ctx)
evalSch input  ctx | output == [] =  (84, (SchString "something went wrong"), ctx)
                   | snd (head output) /= "" = (84, (SchString "Did Not Parse Everything"), ctx)
                   | True = (direct_env (eval (fst $ head output) ctx) ctx)
                        -- where output = parse scheme input
                          where output =  parse atom input


isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0



stripChars :: String -> String -> String
stripChars = filter . flip notElem


schPrint :: SchVal -> String
schPrint (SchNumber i) = show i
schPrint (SchFloat i) | isInt i 5 = show (round i)
                      | otherwise = show i  
schPrint (SchString s) = show s
schPrint (SchQuote q) = q
schPrint (SchEmpty ()) = "()"
schPrint (SchQList []) = "()"
-- schPrint (SchQList ( q: []))  = q ++ ")" 
-- schPrint (SchQList (q:l))  =  q ++ " " ++ schPrint (SchQList l)
schPrint (Error s) = s
schPrint (SchBool b) | b == True = "#t"
                     | otherwise = "#f"
                     
schPrint (Closure _ _ _) = "#<procedure>"
schPrint (Carr e) = schPrint e 
-- |isPair e = schPrint e
-- --                  | otherwise = 
schPrint (DottedList l) = "(" ++ concatDotted "" l ++ ")"
schPrint (Cdrr e) = schPrint e

schPrint (DottedPair a b)
                  | isPair b = schPrint (DottedList [a, b])
--  isPair b = "(" ++ schPrint a ++ build b
                  --  isPair b = schPrint a ++ " " ++ schPrint (Unbraced b)
                  | b == (Cdrr(SchQList [])) = "(" ++ schPrint a ++ ")"
                  | otherwise = "(" ++ schPrint a ++ " . " ++ schPrint b++ ")" 

schPrint (Unbraced val) = schPrint val
  

specialUnbracedPrint:: SchVal -> String
specialUnbracedPrint (Carr (v))                 = schPrint (v)
specialUnbracedPrint (Cdrr (DottedPair v1 v2))
                    | v2 == (Cdrr(SchEmpty ()))    = schPrint v1
                    | v2 == (Cdrr(SchQList [])) = schPrint v1
                    | isPair v2 == False        = schPrint v1 ++ " . " ++ schPrint v2
                    | otherwise                 = schPrint v1 ++ " " ++ specialUnbracedPrint (v2)
specialUnbracedPrint _                          = ""  


concatDotted :: String -> [SchVal] -> String
concatDotted base (l:[]) | base == "" = base ++ specialUnbracedPrint l
                         | otherwise = base ++ " " ++ specialUnbracedPrint l
concatDotted base (l:lx) = concatDotted (base ++ specialUnbracedPrint l) (lx)


isPair :: SchVal -> Bool
isPair (Carr (DottedPair a b)) = True
isPair (Cdrr (DottedPair a b)) = True
isPair (Unbraced (Cdrr (DottedPair a b))) = True
isPair (Unbraced (Carr (DottedPair a b))) = True
isPair _ = False


repl:: Ctx -> IO ()
repl ctx = 
  putStrLn "Chez Scheme Version 9.5.5\nCopyright 1984-2020 Cisco Systems, Inc.\n"
   >> runInputT defaultSettings (loop ctx)
   where
       loop:: Ctx -> InputT IO () 
       loop ctx = do
                -- outputStrLn "THIS LINE IS PRINTED BEFORE EACH OUTPUT"
                minput <- getInputLine "> "
                case minput of
                    Nothing -> outputStrLn "" >> return ()
                    Just "quit" -> return ()
                    --Just input -> do outputStrLn $ "Input was: " ++ input
                    Just input ->
                                   (outputStrLn $ schPrint output) >>
                                   loop context
                                   where (code, output, context) = evalSch (stripChars "\t\n\r" $ input) ctx