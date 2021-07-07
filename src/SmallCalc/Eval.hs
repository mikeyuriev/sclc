module SmallCalc.Eval
    ( evalExpr
    , execStatement
    ) where

import Control.Monad (join, liftM2)
import Data.Fixed (mod')
import SmallCalc.Context
import SmallCalc.Types

binaryOp :: BinaryOp -> Number -> Number -> Either Error Number
binaryOp Div _ 0 = Left EDivisionByZero
binaryOp Mod _ 0 = Left EDivisionByZero
binaryOp op  x y = Right (operation op x y)
    where
        operation Add = (+)
        operation Sub = (-)
        operation Mul = (*)
        operation Div = (/)
        operation Mod = mod'

unaryOp :: UnaryOp -> Number -> Either Error Number
unaryOp Negate = Right . negate

evalExpr :: Context -> Expr -> Either Error Number
evalExpr _   (Constant c)        = Right c
evalExpr ctx (UnaryOp op e)      = unaryOp op =<< evalExpr ctx e
evalExpr ctx (BinaryOp op le re) = join $ liftM2 o l r
    where
        o = binaryOp op
        l = evalExpr ctx le
        r = evalExpr ctx re
evalExpr ctx (Symbol name)       = getSymbol ctx name

execStatement :: Context -> Statement -> Either Error Context
execStatement ctx@(Context syms _) (Out expr)
    = Context syms <$> evalExpr ctx expr
execStatement ctx (Assign name expr)
    = putSymbol ctx name <$> evalExpr ctx expr
