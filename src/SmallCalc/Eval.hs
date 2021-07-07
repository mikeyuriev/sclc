module SmallCalc.Eval
    ( execStatement
    , evalExpr
    ) where

import Data.Fixed (mod')
import SmallCalc.Context
import SmallCalc.Types

binaryOp :: BinaryOp -> Either Error Number -> Either Error Number -> Either Error Number
binaryOp Div _ (Right 0) = Left EDivisionByZero
binaryOp Mod _ (Right 0) = Left EDivisionByZero
binaryOp Add x y = fmap (+) x <*> y
binaryOp Sub x y = fmap (-) x <*> y
binaryOp Mul x y = fmap (*) x <*> y
binaryOp Div x y = fmap (/) x <*> y
binaryOp Mod x y = fmap mod' x <*> y

unaryOp :: UnaryOp -> Either Error Number -> Either Error Number
unaryOp Negate = fmap (* (-1.0))

evalExpr :: Context -> Expr -> Either Error Number
evalExpr _ (Constant c)
    = pure c
evalExpr ctx (UnaryOp op e)
    = unaryOp op (evalExpr ctx e)
evalExpr ctx (BinaryOp op le re)
    = binaryOp op (evalExpr ctx le) (evalExpr ctx re)
evalExpr ctx (Symbol name)
    = getSymbol ctx name

execStatement :: Context -> Statement -> Either Error Context
execStatement ctx@(Context syms _) (Out expr)
    = Context syms <$> evalExpr ctx expr
execStatement ctx (Assign name expr)
    = putSymbol ctx name <$> evalExpr ctx expr
