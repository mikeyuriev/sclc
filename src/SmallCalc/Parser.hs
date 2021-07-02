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
expr =  many space *> term `chainl1` lexeme addOp

term :: Parser Node
term = factor `chainl1` lexeme mulOp

factor :: Parser Node
factor = (lexeme unaryOp <*> factor) <|> lexeme parens <|> lexeme number

lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany space

parens :: Parser Node
parens =
    between oParen cParen expr
    where
        oParen = char '(' <?> "open parenthesis '('"
        cParen = char ')' <?> "closing parenthesis ')'"

number :: Parser Node
number = Value . Constant <$> fmap read num
    where
        num :: Parser String
        num = (++) <$> digits <*> decimal <?> "number"

        decimal :: Parser String
        decimal = option "" $ (:) <$> dot <*> digits

        digits :: Parser String
        digits = many1 (digit <?> "digit")
        dot :: Parser Char

        dot = char '.' <?> "decimal dot '.'"

addOp :: Parser (Node -> Node -> Node)
addOp =
        (char '+' $> BinaryOp Add <?> "addition operator '+'")
    <|> (char '-' $> BinaryOp Sub <?> "subtraction operator '-'")

mulOp :: Parser (Node -> Node -> Node)
mulOp =
        (char '*' $> BinaryOp Mul <?> "multiplication operator '*'")
    <|> (char '/' $> BinaryOp Div <?> "division operator '/'")
    <|> (char '%' $> BinaryOp Mod <?> "modulo operator '%'")

unaryOp :: Parser (Node -> Node)
unaryOp =
    (char '-' <?> "unary minus operator '-'") $> UnaryOp Negate
