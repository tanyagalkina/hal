module Repl
( repl
  ,justTest
  ,evalLispExpression
  ,evalLisp
) where

import System.Console.Haskeline
import System.Exit
import Lexer (fromPairs, Parser, parse)
--import System.Environment (getArgs)


justTest :: Int -> [Int]
justTest n = [n, n]


-- printResult:: [(String, String)] -> IO ()
-- printResult [] = braveExit "Something went wrong .. the output is empty"  84
-- printResult res | snd (head res) /= "" =  
--                         braveExit ("I could not parse everything.. " ++ snd (head res) ++ fst (head res)) 84
                        
--                 | otherwise =
--                    putStrLn $ fst (head res)   

 
evalLisp :: String -> String -> (Int, String, String)
evalLisp input ctx | output == [] = (84, "something went wrong!", ctx)
                   | snd (head output) /= "" = (84, "could not", "parse everything")
                   | True = (0, (fst (head output)), ctx ++ "HA!")
                          where output = parse fromPairs input
                                 
--evalLispExpression input ctx  = fst (head (parse fromPairs input))
--evalLisp input ctx  = ((fst (head (parse fromPairs input))), ctx ++ "a")



evalLispExpression :: String -> String -> (String, String)
--evalLispExpression input ctx  = fst (head (parse fromPairs input))
evalLispExpression input ctx  = ((fst (head (parse fromPairs input))), ctx ++ "a")


repl:: String -> IO ()
repl ctx = runInputT defaultSettings (loop ctx)
   where
       loop:: String -> InputT IO () 
       loop ctx = do
                --outputStrLn ctx === FOR CHECKING THE CONTEXT!! 
                minput <- getInputLine "% "
                case minput of
                    Nothing -> return ()
                    Just "quit" -> return ()
                    --Just input -> do outputStrLn $ "Input was: " ++ input
                    Just input -> do
                                let res = evalLispExpression input ctx
                                outputStrLn $ fst res 
                                loop $ snd res
                                        
