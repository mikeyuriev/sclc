module SmallCalc.Eval
    ( eval
    )
where

import Data.Fixed
import SmallCalc.AST

binaryOp :: BinaryOp -> (Float -> Float -> Float)
binaryOp Add = (+)
binaryOp Sub = (-)
binaryOp Mul = (*)
binaryOp Div = (/)
binaryOp Mod = mod'

unaryOp :: UnaryOp -> (Float -> Float)
unaryOp Negate = (* (-1.0))

eval :: Node -> Float
eval (Value (Constant c)) = c
eval (UnaryOp op v)       = unaryOp op (eval v)
eval (BinaryOp op lv rv)  = binaryOp op (eval lv) (eval rv)

