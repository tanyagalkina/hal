module Repl
( repl
  ,justTest
  ,evalLispExpression
  ,evalLisp
) where

import Debug.Trace
import System.Console.Haskeline
import System.Exit
import Lexer (fromPairs, Parser, parse)
import Types
--import System.Environment (getArgs)


justTest :: Int -> [Int]
justTest n = [n, n]

evalLisp :: String -> Ctx -> (Int, String, Ctx)
evalLisp input ctx | output == [] = (84, "something went wrong!", ctx)
                   | snd (head output) /= "" = (84, "could not parse everything!", ctx)
                   | True = (0, (fst (head output)), ctx)
                          --TRACING CONTEXT EXAMPLE
                          --where output = traceShow ctx parse fromPairs input
                          where output = parse fromPairs input
                                 
--evalLispExpression input ctx  = fst (head (parse fromPairs input))
--evalLisp input ctx  = ((fst (head (parse fromPairs input))), ctx ++ "a")



evalLispExpression :: String -> String -> (String, String)
--evalLispExpression input ctx  = traceShow ctx fst (head (parse fromPairs input))
evalLispExpression input ctx  = traceShow ctx ((fst (head (parse fromPairs input))), ctx ++ "a")


repl:: Ctx -> IO ()
repl ctx = runInputT defaultSettings (loop ctx)
   where
       loop:: Ctx -> InputT IO () 
       loop ctx = do
                --outputStrLn ctx === FOR CHECKING THE CONTEXT!!
                minput <- getInputLine "% "
                case minput of
                    Nothing -> do
                              outputStrLn ""
                              return ()
                    Just "quit" -> return ()
                    --Just input -> do outputStrLn $ "Input was: " ++ input
                    Just input -> do
                                let (_, output, context) = evalLisp input ctx
                                outputStrLn output 
                                loop context
                                        
