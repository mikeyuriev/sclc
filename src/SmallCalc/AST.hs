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

newtype Value
    = Constant Double
    deriving (Eq, Show)

data Node
    = Value Value
    | BinaryOp BinaryOp Node Node
    | UnaryOp UnaryOp Node
    deriving (Eq, Show)
