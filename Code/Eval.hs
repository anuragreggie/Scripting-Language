module Eval where

import Parsing
import System.IO
import Tree
import Data.Char

-- These are the REPL commands
data Command
  = Set Name Expr -- assign an expression to a variable name
  | Print Expr -- evaluate an expression and print the result
  | Quit
  | Input Expr
  | Read Name
  | ParseFailure
  | FileNotFound
  | If Expr [Command] [Command] -- If Bool Then Else
  | Repeat Int [Command] -- repeat a list of commands n times.
  | While Expr [Command]
  | For Command Expr [Command] [Command]
  deriving (Show)

-- These are the possible Expressions the user can use.
data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Pow Expr Expr
  | ToString Expr
  | ToInt Expr
  | Abs Expr
  | Val Double
  | Bool Bool
  | Var Name
  | Str String
  | Concat Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Eq Expr Expr
  | NEq Expr Expr
  | GtEq Expr Expr
  | LtEq Expr Expr
  | Gt Expr Expr
  | Lt Expr Expr
  | Not Expr
  deriving (Show)

-- This function is used to Evaluate a given Expr
eval ::
  Tree -> -- Variable name to value mapping
  Expr -> -- Expression to evaluate
  Either Error Value -- Result (if no errors such as missing variables)
  -- Int eval
eval vars (Val x) = Right $ DoubleVal x -- for values, Right give the value directly
eval vars (Var x) = getVar x vars
eval vars (Add x y) = eval vars x `add` eval vars y
eval vars (Sub x y) = eval vars x `minus` eval vars y
eval vars (Mul x y) = eval vars x `mul` eval vars y
eval vars (Div x y) = eval vars x `di` eval vars y
eval vars (Mod x y) = eval vars x `modulo` eval vars y
eval vars (Pow x y) = eval vars x `power` eval vars y
eval vars (Abs x) = absolute $ eval vars x
-- String Eval
eval vars (Str x) = Right $ StrVal x
eval vars (Concat x y) = eval vars x `conc` eval vars y
eval vars (ToString x) = stringify $ eval vars x
eval vars (ToInt x) = doubleify $ eval vars x
-- Boolean Eval
eval vars (Bool b) = Right $ BoolVal b
eval vars (Eq x y) = comp (eval vars x) (eval vars y) EQ
eval vars (Gt x y) = comp (eval vars x) (eval vars y) GT
eval vars (Lt x y) = comp (eval vars x) (eval vars y) LT
eval vars (GtEq x y) = comp (eval vars x) (eval vars y) EQ `orB` comp (eval vars x) (eval vars y) GT
eval vars (LtEq x y) = comp (eval vars x) (eval vars y) EQ `orB` comp (eval vars x) (eval vars y) LT
eval vars (NEq x y) = notB $ comp (eval vars x) (eval vars y) EQ
eval vars (And x y) = andB (eval vars x) (eval vars y)
eval vars (Or x y) = orB (eval vars x) (eval vars y)
eval vars (Not x) = notB (eval vars x)

{-
These functions are used to perform integer operations for two given Either Error Values(Int) and return the relevent results as a Either Error Value
-}

-- Modified helper function from https://stackoverflow.com/questions/46675016/haskell-round-number
round4dp :: Double -> Double
round4dp x = fromIntegral (round $ x * 1e4) / 1e4

-- This adds together two Either Double Val
add :: Either Error Value -> Either Error Value -> Either Error Value
add (Right (DoubleVal x)) (Right (DoubleVal y)) = Right $ DoubleVal $ round4dp $ x + y
add _ _ = Left AddError

-- This subtracts two Either Double Val
minus :: Either Error Value -> Either Error Value -> Either Error Value
minus (Right (DoubleVal x)) (Right (DoubleVal y)) = Right $ DoubleVal $ round4dp $ x - y
minus _ _ = Left SubError

-- This Multiplys two Either Double Vals
mul :: Either Error Value -> Either Error Value -> Either Error Value
mul (Right (DoubleVal x)) (Right (DoubleVal y)) = Right $ DoubleVal $ round4dp $ x * y
mul _ _ = Left MulError

-- This Divides two Either Double Vals
di :: Either Error Value -> Either Error Value -> Either Error Value
di (Right (DoubleVal x)) (Right (DoubleVal y)) = Right $ DoubleVal $ round4dp $ x / y
di _ _ = Left DivError

-- This reduces a Either Double Val Modulo another Either Double Val
modulo :: Either Error Value -> Either Error Value -> Either Error Value
modulo (Right (DoubleVal x)) (Right (DoubleVal y)) = Right $ DoubleVal $ fromIntegral $ round x `mod` round y
modulo _ _ = Left ModError

-- This find the power of a Either Double Val to another Either Double Val
power :: Either Error Value -> Either Error Value -> Either Error Value
power (Right (DoubleVal x)) (Right (DoubleVal y))
  | x < 0 = Right $ DoubleVal $ round4dp $ (-1) * ((x * (-1)) ** y)
  | otherwise = Right $ DoubleVal $ round4dp $ x ** y
power _ _ = Left PowError

-- This calculates the absolute value of a Either Double Val
absolute :: Either Error Value -> Either Error Value
absolute (Right (DoubleVal x)) = Right $ DoubleVal $ abs x
absolute _ = Left AbsError

{-
These functions are used to perform String operations for two given Either Error Values(String) and return the relevent results as a Either Error Value
-}
-- This will convert Either Boolval and Either DoubleVal to a Either StrVal
stringify :: Either Error Value -> Either Error Value
stringify (Right (DoubleVal x)) = Right $ StrVal $ show x
stringify (Right (BoolVal x)) = Right $ StrVal $ show x
stringify _ = Left ToStringError

-- This will convert Either DoubleVval and Either StrVal to a Either DoubleVal
doubleify :: Either Error Value -> Either Error Value
doubleify (Right (StrVal x)) 
                      | all isDigit x = Right $ DoubleVal (read x :: Double) -- This ensures that the string passed can be read to a Double.
                      | otherwise = Left ToIntError 
doubleify (Right (DoubleVal x)) = Right $ DoubleVal x
doubleify _ = Left ToIntError

--  This will concatanate two Either StringVals and increase a DoubleVal by 1
conc :: Either Error Value -> Either Error Value -> Either Error Value
conc (Right (StrVal x)) (Right (StrVal y)) = Right $ StrVal $ x ++ y
conc (Right (DoubleVal x)) _ = Right $ DoubleVal $ x + 1 -- This allows values to be incremented by 1 e.g. 1++
conc _ _ = Left ConcatError

{-
These functions are used to perform Boolean operations for two given Either Error Values(Boolean) and return the relevent results as a Either Error Value
-}

-- This will compare two Either Error values and return a bool val depending on if the ordering matches the one passed to the function.
comp :: Either Error Value -> Either Error Value -> Ordering -> Either Error Value
comp x y ord = do
  let ord2 = x `com` y
  bVal (Right ord) ord2
  where
    com (Right x) (Right y) = Right $ x `compare` y
    com _ _ = Left Empty
    bVal _ (Left Empty) = Left Empty
    bVal o1 o2 = Right $ BoolVal $ o1 == o2

-- This function will perform a boolean and on two Either BoolVal
andB :: Either Error Value -> Either Error Value -> Either Error Value
andB (Right (BoolVal x)) (Right (BoolVal y)) = Right $ BoolVal (x && y)
andB _ _ = Left CompError

-- This function will perform a boolean r on two Either BoolVal
orB :: Either Error Value -> Either Error Value -> Either Error Value
orB (Right (BoolVal x)) (Right (BoolVal y)) = Right $ BoolVal (x || y)
orB _ _ = Left CompError

-- This function will perform a boolean not on a Either BoolVal
notB :: Either Error Value -> Either Error Value
notB (Right (BoolVal x)) = Right $ BoolVal (not x)
notB _ = Left CompError