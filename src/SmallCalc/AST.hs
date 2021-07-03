module SmallCalc.AST where

data BinaryOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    deriving (Eq, Show)

data UnaryOp
    = Negate
    deriving (Eq, Show)

data Node
    = Constant Double
    | BinaryOp BinaryOp Node Node
    | UnaryOp UnaryOp Node
    deriving (Eq, Show)
