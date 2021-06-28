module SmallCalc.AST where

data BinaryOp
    = Add
    | Mul
    deriving (Eq, Show)

data UnaryOp
    = Negate
    deriving (Eq, Show)

newtype Value
    = Constant Float
    deriving (Eq, Show)

data Node
    = Value Value
    | BinaryOp BinaryOp Node Node
    | UnaryOp UnaryOp Node
    deriving (Eq, Show)
