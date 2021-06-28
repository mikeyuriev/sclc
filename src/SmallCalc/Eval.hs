module SmallCalc.Eval
    ( eval
    )
where

import SmallCalc.AST

binaryOp :: BinaryOp -> (Float -> Float -> Float)
binaryOp Add = (+)
binaryOp Mul = (*)

unaryOp :: UnaryOp -> (Float -> Float)
unaryOp Negate = (* (-1.0))

eval :: Node -> Float
eval (Value (Constant c)) = c
eval (UnaryOp op v)       = unaryOp op (eval v)
eval (BinaryOp op lv rv)  = binaryOp op (eval lv) (eval rv)

