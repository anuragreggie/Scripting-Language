module Expr where

import Eval
import Parsing
import System.IO

{-
This File is used to parse Commands and Expressions used by the
user and return the relevent data type representation.
-}

-- This function is used to parse all commands the user can use.
pCommand :: Parser Command
pCommand =
  do
    string "print" -- Print Command
    Print <$> pExpr
    ||| do
      string "quit" -- Quit Program
      return Quit
    ||| do
      string "read" -- Reading from file
      a <- ident
      char '.'
      c <- ident
      let fName = a ++ "." ++ c
      return (Read fName)
    ||| do
      string "if" -- If Statement
      b <- pFactor
      string "then{"
      th <- many1 pBlockCommand
      string "}else{"
      el <- many1 pBlockCommand
      char '}'
      return (If b th el)
    ||| do
      t <- many letter -- Assigning Varable user input
      char '='
      string "input"
      return (Input (Var t))
    ||| do
      t <- many letter -- Assigning Varable values
      char '='
      Set t <$> pExpr
    ||| do
      string "repeat" -- Repeat command
      n <- int
      do char '{'
      cmds <- do many1 pBlockCommand
      do char '}'
      return (Repeat n cmds)
    ||| do
      string "while" -- While loop
      b <- pFactor
      char '{'
      cmds <- do many1 pBlockCommand
      char '}'
      return (While b cmds)
    ||| do
      string "for(" -- For loop
      d <- pCommand
      char ';'
      b <- pExpr
      char ';'
      ex <- many1 pBlockCommand
      string "){"
      cmds <- many1 pBlockCommand
      char '}'
      return $ For d b ex cmds
    ||| do pCommandShorthand

-- This will parse legal Exprsssion and return the relevent Expr data type.
pExpr :: Parser Expr
pExpr = do
  t <- pTerm
  do
    string "++" -- String Concatanation / Shorthand to increment var
    Concat t <$> pExpr
    ||| do
      string "--" -- Shorthand to decrement var
      return $ Sub t (Val 1)
    ||| do
      string "+=" -- Shorthand to increment var
      Add t <$> pFactor
    ||| do
      string "-=" -- Shorthand to decrement var
      Sub t <$> pFactor
    ||| do
      string "||" -- Boolean Or
      Or t <$> pExpr
    ||| do
      string "&&" -- Boolean and
      And t <$> pExpr
    ||| do
      char '+' -- Addition
      Add t <$> pExpr
    ||| do
      char '-' -- Minus
      Sub t <$> pExpr
    ||| return t

--  Parseing the Term of the Expr
pTerm :: Parser Expr
pTerm = do
  f <- pFactor
  do
    char '^' -- Power
    Pow f <$> pTerm
    ||| do
      char '*' -- Multiply
      Mul f <$> pTerm
    ||| do
      char '/' -- Divide
      Div f <$> pTerm
    ||| do
      char '%' -- Modulus
      Mod f <$> pTerm
    ||| do
      b <- pBool -- Boolean Expresion
      b f <$> pFactor
    ||| return f

--  Parseing the Factor of the Expr
pFactor :: Parser Expr
pFactor =
  do
    string "toString" -- To String
    ToString <$> pFactor
    ||| do
      char '"' -- Parsing Strings "Example String"
      str <- many (sat (/= '"'))
      char '"'
      return (Str str)
    ||| do
      char '!' -- Not Boolean
      Not <$> pExpr
    ||| do
      string "toInt" -- To Int
      char '('
      v <- pFactor
      char ')'
      return (ToInt v)
    ||| do
      string "abs" -- Absolute Value
      Abs <$> pFactor
    ||| do
      char '(' -- Parsing paranthesis
      e <- pExpr
      char ')'
      return e
    ||| do Val <$> double -- Double value
    ||| do Bool <$> boolean -- Boolean Value
    ||| do Var <$> many letter -- String value

-- This will create a list of parsed commands.
pBlockCommand :: Parser Command
pBlockCommand = do
  cmd <- pCommand
  do char ';'
  return cmd

-- This is used to parse shorthand versions of commands e.g. x++
pCommandShorthand :: Parser Command
pCommandShorthand = do
  t <- many letter
  do
    string "++" -- Shorthand to increment var
    return $ Set t (Add (Var t) (Val 1))
    ||| do
      string "--" -- Shorthand to decrement var
      return $ Set t (Sub (Var t) (Val 1))
    ||| do
      string "+=" -- Shorthand to increment var
      Set t . Add (Var t) . Val <$> double
    ||| do
      string "-=" -- Shorthand to decrement var
      Set t . Sub (Var t) . Val <$> double

-- This is used to parse bool expresions.
pBool :: Parser (Expr -> Expr -> Expr)
pBool =
  do
    string "==" -- Boolean Equal
    return Eq
    ||| do
      string "!=" -- Not Equal
      return NEq
    ||| do
      string ">=" -- Greater or Equal
      return GtEq
    ||| do
      string "<=" -- Less or Equal
      return LtEq
    ||| do
      string ">" -- Greater Than
      return Gt
    ||| do
      string "<" -- Less Than
      return Lt

-- This is used to remove any uneeded whitespace from the input string
trimWhiteSpace :: [Char] -> [Char]
trimWhiteSpace st = trimmed st False
  where
    trimmed [] _ = []
    trimmed (c : st) inStr
      | c == ' ' && not inStr = trimmed st inStr
      | c == '\"' = c : trimmed st (not inStr)
      | otherwise = c : trimmed st inStr

-- Gets the head of parsed list returns maybe value safe version of the result.
getParseResult :: [(Expr, [Char])] -> Expr
getParseResult [(x, "")] = x
getParseResult _ = undefined