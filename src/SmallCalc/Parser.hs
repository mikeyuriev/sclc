module SmallCalc.Parser
    ( line
    , expr
    )
where

import Data.Functor
import Text.Parsec
import Text.Parsec.String
import SmallCalc.AST

line :: Parser Node
line = expr <* eof

expr :: Parser Node
expr = term `chainl1` addOp

term :: Parser Node
term = unary `chainl1` mulOp

unary :: Parser Node
unary = (unaryOp <*> factor) <|> factor

factor :: Parser Node
factor = parens <|> number

parens :: Parser Node
parens = between (char '(') (char ')') expr

number :: Parser Node
number = Value . Constant <$> num
    where
        num :: Parser Float
        num = fmap read $ (++) <$> integer <*> decimal
        integer :: Parser String
        integer = many1 digit
        decimal :: Parser String
        decimal = option "" $ (:) <$> char '.' <*> many1 digit

addOp :: Parser (Node -> Node -> Node)
addOp =
    char '+' $> BinaryOp Add

mulOp :: Parser (Node -> Node -> Node)
mulOp =
    char '*' $> BinaryOp Mul

unaryOp :: Parser (Node -> Node)
unaryOp =
    char '-' $> UnaryOp Negate

