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
line = expr <* (eof <?> "end of input")

expr :: Parser Node
expr =  many (space <?> "") *> term `chainl1` lexeme addOp

term :: Parser Node
term = factor `chainl1` lexeme mulOp

factor :: Parser Node
factor =
        (lexeme unaryOp <*> factor)
    <|> lexeme parens
    <|> lexeme constant

lexeme :: Parser a -> Parser a
lexeme p = p <* skipMany (space <?> "")

parens :: Parser Node
parens =
    between oParen cParen expr <?> "(expression)"
    where
        oParen = char '(' <?> "("
        cParen = char ')' <?> ")"

constant :: Parser Node
constant = Constant <$> fmap read num
    where
        num :: Parser String
        num = (++) <$> digits <*> decimal <?> "number"

        decimal :: Parser String
        decimal = option "" $ (:) <$> dot <*> digits

        digits :: Parser String
        digits = many1 (digit <?> "digit")
        dot :: Parser Char

        dot = char '.' <?> "."

addOp :: Parser (Node -> Node -> Node)
addOp =
        (char '+' $> BinaryOp Add <?> "+")
    <|> (char '-' $> BinaryOp Sub <?> "-")

mulOp :: Parser (Node -> Node -> Node)
mulOp =
        (char '*' $> BinaryOp Mul <?> "*")
    <|> (char '/' $> BinaryOp Div <?> "/")
    <|> (char '%' $> BinaryOp Mod <?> "%")

unaryOp :: Parser (Node -> Node)
unaryOp =
    (char '-' <?> "-") $> UnaryOp Negate
