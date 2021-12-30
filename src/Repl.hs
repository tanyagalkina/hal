module Repl
( repl
  ,schPrint
  ,evalSch 
) where

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


schPrint :: SchVal -> String
schPrint (SchNumber i) = show i
schPrint (SchFloat i) | isInt i 5 = show (round i)
                      | otherwise = show i  
schPrint (SchString s) = show s
schPrint (SchQuote q) = q
schPrint (SchQList []) = "()"
schPrint (SchQList ( q: []))  = q ++ ")" 
schPrint (SchQList (q:l))  =  q ++ " " ++schPrint (SchQList l)
schPrint (Error s) = s
schPrint (SchBool b) | b == True = "#t"
                     | otherwise = "#f"
                     
schPrint (Closure _ _ _) = "#<procedure>"
schPrint (Carr e) = schPrint e 
-- |isPair e = schPrint e
-- --                  | otherwise = 
schPrint (DottedList list) = "(" ++ schPrint list
schPrint (Cdrr e) = schPrint e

schPrint (Unbraced (DottedPair a b)) | isPair b = schPrint a ++ " " ++ schPrint (Unbraced b)
                                     | (b == (Cdrr(SchQList []))) = schPrint (Unbraced a)
                                     | otherwise = schPrint a ++ " . " ++ schPrint b 

schPrint (DottedPair a b)
--  isPair b = "(" ++ schPrint a ++ build b
                  | isPair b = schPrint a ++ " " ++ schPrint (Unbraced b)
                  | b == (Cdrr(SchQList [])) = schPrint a ++ ")"
                  | otherwise = "(" ++ schPrint a ++ " . " ++ schPrint b++ ")" 

schPrint (Unbraced val) = schPrint val
  

--- THI SI ONLY NEED FOR REPRESENTATION! THE STRCUCTURE CAR CDR IS CORRECT!!  
-- build :: SchVal -> String
-- build (DottedPair a b) = schPrint a ++ schPrint b   

isPair :: SchVal -> Bool
--isPair pair = traceShow pair True
isPair (Carr (DottedPair a b)) = True
isPair (Cdrr (DottedPair a b)) = True
isPair (Unbraced (Cdrr (DottedPair a b))) = True
isPair (Unbraced (Carr (DottedPair a b))) = True
isPair _ = False


repl:: Ctx -> IO ()
repl ctx = runInputT defaultSettings (loop ctx)
   where
       loop:: Ctx -> InputT IO () 
       loop ctx = do
                --outputStrLn ctx === FOR CHECKING THE CONTEXT!!
                minput <- getInputLine "% "
                case minput of
                    Nothing -> outputStrLn "" >> return ()
                    Just "quit" -> return ()
                    --Just input -> do outputStrLn $ "Input was: " ++ input
                    Just input ->
                                   (outputStrLn $ schPrint output) >>
                                   loop context
                                   where (code, output, context) = evalSch input ctx