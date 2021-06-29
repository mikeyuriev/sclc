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
expr = skipSpaces *> term `chainl1` addOp

term :: Parser Node
term = unary `chainl1` mulOp

unary :: Parser Node
unary = (unaryOp <*> expr) <|> factor

factor :: Parser Node
factor = parens <|> number


skipSpaces :: Parser ()
skipSpaces = skipMany space

parens :: Parser Node
parens =
    between oParen cParen expr
    <* skipSpaces
    where
        oParen = char '(' <?> "open parenthesis '('"
        cParen = char ')' <?> "closing parenthesis ')'"

number :: Parser Node
number = Value . Constant <$> fmap read num <* skipSpaces
    where
        num :: Parser String
        num = (++) <$> integer <*> decimal
        integer :: Parser String
        integer = digits
        decimal :: Parser String
        decimal = option "" $ (:) <$> dot <*> digits
        digits :: Parser String
        digits = many1 (digit <?> "digit")
        dot :: Parser Char
        dot = char '.' <?> "decimal dot '.'"

addOp :: Parser (Node -> Node -> Node)
addOp =
    (
        (char '+' $> BinaryOp Add <?> "addition operator '+'")
    <|> (char '-' $> BinaryOp Sub <?> "subtraction operator '-'")
    )
    <* skipSpaces

mulOp :: Parser (Node -> Node -> Node)
mulOp =
    (
        (char '*' $> BinaryOp Mul <?> "multiplication operator '*'")
    <|> (char '/' $> BinaryOp Div <?> "division operator '/'")
    <|> (char '%' $> BinaryOp Mod <?> "modulo operator '%'")
    )
    <* skipSpaces

unaryOp :: Parser (Node -> Node)
unaryOp =
    (char '-' <?> "unary minus operator '-'") $> UnaryOp Negate
