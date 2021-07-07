module SmallCalc.Parser.Internal where

import Control.Arrow (left)
import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.String

import SmallCalc.Error.Internal
import SmallCalc.Types

parseStatement :: String -> Either Error Statement
parseStatement = left fromParseError . parse statement ""

statement :: Parser Statement
statement = ignored *> (assign <|> out) <* end

assign :: Parser Statement
assign = Assign <$> try (lexeme name <* lexeme delim) <*> expr
    where
        delim = char '=' <?> "="

out :: Parser Statement
out = Out <$> expr

expr :: Parser Expr
expr = ignored *> term `chainl1` lexeme addOp

term :: Parser Expr
term = factor `chainl1` lexeme mulOp

factor :: Parser Expr
factor
    =   (lexeme unaryOp <*> factor)
    <|> lexeme parens
    <|> lexeme (Constant <$> number)
    <|> lexeme (Symbol <$> name)

lexeme :: Parser a -> Parser a
lexeme p = p <* ignored

parens :: Parser Expr
parens =
    between oParen cParen expr <?> "(expression)"
    where
        oParen = char '(' <?> "("
        cParen = char ')' <?> ")"

number :: Parser Number
number = fmap read num <?> "number"
    where
        num :: Parser String
        num = (++) <$> digits <*> decimal

        decimal :: Parser String
        decimal = option "" $ (:) <$> dot <*> digits

        digits :: Parser String
        digits = many1 (digit <?> "digit")
        dot :: Parser Char

        dot = char '.' <?> "."

name :: Parser Name
name = many1 (oneOf (['A'..'Z'] ++ ['a' .. 'z'])) <?> "name"

addOp :: Parser (Expr -> Expr -> Expr)
addOp
    =   (char '+' $> BinaryOp Add <?> "+")
    <|> (char '-' $> BinaryOp Sub <?> "-")

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp
    =   (char '*' $> BinaryOp Mul <?> "*")
    <|> (char '/' $> BinaryOp Div <?> "/")
    <|> (char '%' $> BinaryOp Mod <?> "%")

unaryOp :: Parser (Expr -> Expr)
unaryOp = (char '-' <?> "-") $> UnaryOp Negate

ignored :: Parser ()
ignored = spaces <?> ""

end :: Parser ()
end = eof <?> "end of input"
