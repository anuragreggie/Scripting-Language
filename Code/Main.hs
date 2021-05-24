module Main where
import REPL
import System.Console.Haskeline
import System.IO

-- This will Start the main REPL loop using Haskline.
main :: IO ()
main = do writeFile inputHistoryFile "" -- Creating a new file to store use input history.
          putStr ""
          hFlush stdout
          runInputT defaultSettings (repl initState)
