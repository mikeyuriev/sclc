module SmallCalc.Eval
    ( EvalResult
    , evalParseResult
    , eval
    )
where

import Control.Arrow (left)
import Data.Fixed (mod')
import Text.Parsec.Error (ParseError)
import SmallCalc.AST
import SmallCalc.Error

type EvalResult = Either Error Double

binaryOp :: BinaryOp -> EvalResult -> EvalResult -> EvalResult
binaryOp Div _ (Right 0) = Left DivisionByZero
binaryOp Mod _ (Right 0) = Left DivisionByZero
binaryOp Add x y = fmap (+) x <*> y
binaryOp Sub x y = fmap (-) x <*> y
binaryOp Mul x y = fmap (*) x <*> y
binaryOp Div x y = fmap (/) x <*> y
binaryOp Mod x y = fmap mod' x <*> y

unaryOp :: UnaryOp -> EvalResult -> EvalResult
unaryOp Negate = fmap (* (-1.0))

evalParseResult :: Either ParseError Node -> EvalResult
evalParseResult node = eval =<< left fromParseError node

eval :: Node -> EvalResult
eval (Constant c)        = pure c
eval (UnaryOp op n)      = unaryOp op (eval n)
eval (BinaryOp op ln rn) = binaryOp op (eval ln) (eval rn)
