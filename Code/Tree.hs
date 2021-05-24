module Tree where

{-
This File is an Implementation of a Binary Search Tree data structure.
The BST is used to store a tuple of Name and value data type.
The tree is ordered using the Name of the tuple. This avoids duplicates.
-}

type Name = String

-- Value Data Type to represent String, Double and Boolean values.
data Value = StrVal String | DoubleVal Double | BoolVal Bool
  deriving (Show, Eq, Ord)

-- Data type to represent BST.
data Tree = Leaf | Node (Name, Value) Tree Tree deriving (Show)

-- This function is used to traverse the BST and return the value of the given Name.
getVar :: Name -> Tree -> Either Error Value
getVar var Leaf = Left Empty -- If the var isn't in the tree a error is returned.
getVar var (Node (node_name, node_val) left right)
  | var == node_name = Right node_val -- Returing the value of the node.
  | var > node_name = getVar var right -- Seaching the right branch.
  | otherwise = getVar var left -- Searching the left branch.

-- This function is used to add a Node to the BTS
addVar :: (Name, Value) -> Tree -> Tree
addVar var Leaf = Node var Leaf Leaf -- Adding a new Node
addVar var (Node (node_name, node_val) left right)
  | fst var == node_name = Node var left right -- This allows reasignment of vars
  | fst var > node_name = Node (node_name, node_val) left (addVar var right) -- Adding to the right
  | otherwise = Node (node_name, node_val) (addVar var left) right -- Adding to the left

-- Error data type to represent the differnt types of program Errors.
data Error
  = Empty
  | AddError
  | SubError
  | DivError
  | MulError
  | ModError
  | PowError
  | AbsError
  | ToStringError
  | ToIntError
  | ConcatError
  | CompError
  deriving (Eq, Ord, Show)