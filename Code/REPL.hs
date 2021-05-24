module REPL where

import Expr
import Parsing
import Eval
import Data.Either
import Tree 
import System.Console.Haskeline
import Control.Monad.IO.Class
import Data.List
import System.Directory

-- This is used to keep track of the differnt varable names and values the user assigns.
data State = State {  vars :: Tree
                    , word_completion :: [String] -- This list is used to store strings to be tab completed when inputting a command.
                     } deriving Show 

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.
repl :: State -> InputT IO ()
repl st = do inp <- runInputT (settings st) (getInputLine "> ")
             case inp of
                Nothing -> return ()
                Just s -> do case parse pCommand $ trimWhiteSpace s of
                                  [(cmd, "")] ->  do process st [cmd]
                                  _ -> do outputStrLn $ processError Empty -- If the parser fails an error is outputed.
                                          repl st


-- This function is used to process a list of commands. 
-- Once all commands are processed the function will call repl to create a loop.
process :: State -> [Command] -> InputT IO ()
process st [] =  repl st
process st (Quit:xs) =  outputStrLn "Quitting" -- Quitting the program.
process st ((Set var e):xs) -- Setting a value e.g. x = 12.
     = do let val = eval (vars st) e
          let st' = addToState var val st
          process st' xs
process st ((Input (Var v)):xs) -- Setting a value  to a input e.g. x = input.
     = do inp <- liftIO getLine 
          case parse pExpr $ trimWhiteSpace inp of
               [(exp, "")] -> do let val = eval (vars st) exp
                                 let st' = addToState v val st
                                 process st' xs
               _ -> do outputStrLn $ processError Empty
                       repl st  
process st ((Print e):xs) -- Printing a var e.g. print (10 + x).
     = do let val = eval (vars st) e
          printVal val
          process st xs
process st ((If ex th el):xs) -- Peforming an if() then{} else{} operation.
     = do if isBool ex st then do
             let Right (BoolVal b) = eval (vars st) ex 
             if b then process st (th ++ xs) else process st (el ++ xs)
          else do  -- Error
             outputStrLn $ processError CompError   
             repl st   
process st ((Read f):xs) -- Using the input from a given file.
     = do exists <- liftIO (doesFileExist f)
          if exists 
             then do expr <- liftIO (readFile' f)
                     let ys = parseAll expr
                     process st (ys ++ xs)
             else process st $ FileNotFound : xs
process st ((ParseFailure:xs)) -- Processing a Parser Failure.
     = do outputStrLn "Parse error"
          process st xs
process st ((FileNotFound:xs)) -- Processing a File not found error.
     = do outputStrLn "File cannot be found in the current directory"
          process st xs
process st ((Repeat n cmds):xs) -- Looping a command.
     = do process st (expand n cmds)
process st ((While con cmd): xs) -- While loops
     = do if isBool con st then do -- Checking condition is legal
               let Right (BoolVal b) = eval (vars st) con
               if b then process st (cmd ++ [While con cmd] ++ xs) else process st xs -- Processing list of commands with a new while command at the end to check condition.
          else do  -- Error
             outputStrLn $ processError CompError   
             repl st   
process st ((For (Set var e) con ex cmd):xs) -- For Loops 
     = do let val = eval (vars st) e
          let st' = addToState var val st
          if isBool con st' then do 
               let Right (BoolVal b) = eval (vars st') con
               let cmd' = cmd ++ ex
               if b then process st' (cmd' ++ [While con cmd'] ++ xs) else process st' xs -- Once var has been set, use while loop command.
          else do  -- Error
             outputStrLn $ processError CompError   
             repl st 
          return undefined 


-- This is used to add a given var name and value to the program state to be used later.
addToState :: Name -> Either Error Value -> State -> State
addToState var (Right val) st = st {vars = addVar (var, val) (vars st), word_completion = word_completion st ++ [var]}                   
addToState var _ st = st


-- Function to read all the lines of the file.
readFile' :: Name -> IO ([String])
readFile' f = do contents <- readFile f
                 let linesOfFiles = lines contents
                 return linesOfFiles


-- Parse an array of Strings to produce an array of Commands that can be processed.               
parseAll :: [String] -> [Command]
parseAll inp | isLeft (safeHead inp) = [ParseFailure]
             | otherwise = [if not (null (parse pCommand $ trimWhiteSpace x)) then fst (head (parse pCommand $ trimWhiteSpace x)) else ParseFailure | x <- inp]


-- Safer implementation of the head function.
safeHead :: [a] -> Either Error a 
safeHead []    = Left Empty
safeHead (x:_) = Right x

-- Given an input n and a list, this function expands the list by n.
-- e.g. when n is 2 the list [1,2] becomes [1,2,1,2].
expand :: Int -> [a] -> [a]
expand 1 res = res
expand n res = res ++ expand (n-1) res   

-- These are the setting used for getting the user input using InputT IO
settings :: State -> Settings (InputT IO)
settings st = Settings { historyFile = Just inputHistoryFile
                       , autoAddHistory = True
                      , complete = completeWord Nothing " \t" $ return . completeCom st 
                      }


-- This function is used to return a list of Completion by checking if the input is a prefix of any of the Strings in "word_completion" kept in state.
completeCom ::  State -> String -> [Completion]
completeCom st inp = [simpleCompletion x | x <- word_completion st, inp `isPrefixOf` x]


-- This function is used to print Either Error Values. 
printVal :: Either Error Value -> InputT IO ()
printVal (Right (DoubleVal x)) = outputStrLn $ show x
printVal (Right (StrVal x)) = outputStrLn x
printVal (Right (BoolVal b)) = outputStrLn $ show b
printVal (Left err) = outputStrLn $ processError err

-- Function to process errors and print specific messages.
processError :: Error -> String
processError Empty = "Invalid input"
processError AddError = "Invalid operands used for addition"
processError SubError = "Invalid operands used for subtraction"
processError DivError = "Invalid operands used for division"
processError MulError = "Invalid operands used for multiplication"
processError PowError = "Invalid operands used when raising to a power"
processError AbsError = "Invalid operands used to find the absolute value"
processError ToStringError = "Cannot convert the given input to a string"
processError ToIntError = "Cannot convert the given input to an integer"
processError ConcatError = "Cannot concatenate the given inputs"
processError CompError = "Cannot compare the given values"

-- This holds the inital state to be used when the program starts.
initState :: State
initState = State Leaf [ "if", "else", "then", "print", "toString", "toInt", "quit", "repeat", "for", "while", "for(i = 0; i < ; i++;){}"]

-- This is the file to save inputHistory to. 
inputHistoryFile :: [Char]
inputHistoryFile = "inputHistory"


-- This function is used to check that the passed expression is a legal boolean expression.
isBool :: Expr -> State -> Bool
isBool ex st = check $ eval (vars st) ex
          where check (Right (BoolVal x)) = True 
                check _ = False
