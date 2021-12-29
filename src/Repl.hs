module Repl
( repl
  ,justTest
  -- ,evalLispExpression
  -- ,evalLisp
  ,schPrint 
) where

import Debug.Trace
import System.Console.Haskeline
import System.Exit
import Types
import SchEval
import Grammer

justTest :: Int -> [Int]
justTest n = [n, n]


-- evalLisp :: String -> Ctx -> (Int, String, Ctx)
-- evalLisp input ctx | output == [] = (84, "something went wrong!", ctx)
--                    --  snd (head output) /= "" = (84, "not complete, left::" ++ snd (head output) ++ "we got::"  ++ fst (head output), ctx)
--                    | snd (head output) /= "" = (84, "(" ++ snd (head output) ++ ")", ctx) 
--                    | True = (0, (fst (head output)), ctx)
--                           --TRACING CONTEXT EXAMPLE
--                           where output = traceShow ctx parse fromPairs input
--                           --where output = parse fromPairs input
                                 
-- evalLispExpression :: String -> String -> (String, String)
-- --evalLispExpression input ctx  = traceShow ctx fst (head (parse fromPairs input))
-- evalLispExpression input ctx  = traceShow ctx ((fst (head (parse fromPairs input))), ctx ++ "a")


isInt :: (Integral a, RealFrac b) => b -> a -> Bool
isInt x n = (round $ 10^(fromIntegral n)*(x-(fromIntegral $ round x)))==0


schPrint :: SchVal -> String
schPrint (SchNumber i) = show i
schPrint (SchFloat i) | isInt i 5 = show (round i)
                      | otherwise = show i  
schPrint (SchString s) = show s
schPrint (SchQuote q) = q
schPrint (Error s) = s
schPrint (SchBool b) | b == True = "#t"
                     | otherwise = "#f"
                     


schPrint (Closure _ _ _) = "#<procedure>"
schPrint (DottedList a as) = "(" ++ (schPrint $ head a) ++ (sof as)
      -- where sof | (head as) /= (Quote "()") =  " . " ++ schPrint (head as) ++ ")"
      --   otherwise ")"



sof :: SchVal -> String
sof val | val /= (SchQuote "()") = " . " ++ schPrint val ++ ")"
        | otherwise = ")"

-- concat_qlist :: [SchExpr]-> String -> String
-- concat_qlist (x:[]) base = (SchString $ x) ++ base
-- concat_qlist (x:xs) base = " " ++ (concat_qlist xs ((show x) ++ base))


repl:: Ctx -> IO ()
repl ctx = runInputT defaultSettings (loop ctx)
   where
       loop:: Ctx -> InputT IO () 
       loop ctx = do
                --outputStrLn ctx === FOR CHECKING THE CONTEXT!!
                minput <- getInputLine "% "
                case minput of
                    Nothing -> outputStrLn "" >> return ()
                    -- Nothing -> do
                    --           outputStrLn ""
                    --           return ()
                    Just "quit" -> return ()
                    --Just input -> do outputStrLn $ "Input was: " ++ input
                    Just input ->
                                   (outputStrLn $ schPrint output) >>
                                   loop context
                                   where (code, output, context) = evalSch input ctx

                       --let (code, output, context) = evalSch input ctx >>

                    -- Just input -> do
                    --             let (code, output, context) = evalSch input ctx
                    --             outputStrLn $ schPrint output 
                    --             loop context
                                        
