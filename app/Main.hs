module Main where
import System.Console.Haskeline
import System.Exit

--import Lib

braveExit :: String -> Int -> IO ()
braveExit str 0 = putStrLn str >> exitWith (ExitSuccess)
braveExit str n = putStrLn str >> exitWith (ExitFailure n)


main :: IO ()
main = braveExit "se you later ..." 0


--main ::  IO ()
--main =
--    getArgs >>= \argv ->
--    if length argv == 1 && length (head argv) > 0
--      then printResult $ parse expression $ stripChars " \t" (head argv)
--    else braveExit "share something if u dont mind ..." 84




-- ~
-- THIS PIECE IS FOR INTERACTIVE HAL
{--main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "% "
           case minput of
               Nothing -> return ()
               Just "quit" -> return ()
               Just input -> do outputStrLn $ "Input was: " ++ input
                                loop --}
