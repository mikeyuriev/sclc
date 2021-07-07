module SmallCalc.Types where

type Name = String
type Number = Double

type Symbols = [(Name, Number)]

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

data Expr
    = Constant Number
    | Symbol Name
    | BinaryOp BinaryOp Expr Expr
    | UnaryOp UnaryOp Expr
    deriving (Eq, Show)

data Statement
    = Assign Name Expr
    | Out Expr
    deriving (Eq, Show)

data Error
    = ESyntax Int [String]
    | EDivisionByZero
    | EUndefined Name
    deriving (Eq, Show)

data Context = Context Symbols Number deriving (Eq, Show)
