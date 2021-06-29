module SmallCalc.Eval
    ( eval
    )
where

import Data.Fixed
import SmallCalc.AST

binaryOp :: BinaryOp -> (Double -> Double -> Double)
binaryOp Add = (+)
binaryOp Sub = (-)
binaryOp Mul = (*)
binaryOp Div = (/)
binaryOp Mod = mod'

unaryOp :: UnaryOp -> (Double -> Double)
unaryOp Negate = (* (-1.0))

eval :: Node -> Double
eval (Value (Constant c)) = c
eval (UnaryOp op v)       = unaryOp op (eval v)
eval (BinaryOp op lv rv)  = binaryOp op (eval lv) (eval rv)

