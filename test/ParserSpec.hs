module ParserSpec where

import Test.Hspec
import Text.Parsec

import SmallCalc.Parser.Internal
import SmallCalc.Types

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (expr <* end) ""

parseStmt :: String -> Either ParseError Statement
parseStmt = parse (statement <* end) ""

hasErrorOnPos :: Int -> Either ParseError a -> Bool
hasErrorOnPos x (Left err) = x == (sourceColumn . errorPos $ err)
hasErrorOnPos _ _          = False

spec :: Spec
spec = do
    describe "statement" $ do
        it "should parse assignment" $ do
            parseStmt "name=1" `shouldBe` Right
                ( Assign
                    "name"
                    ( Constant 1.0 )
                )
        it "should parse out" $ do
            parseStmt "1" `shouldBe` Right
                ( Out
                    ( Constant 1.0 )
                )
            parseStmt "name" `shouldBe` Right
                ( Out
                    ( Symbol "name" )
                )
        it "should skip spaces" $ do
            parseStmt "  name  =  1  " `shouldBe` Right
                ( Assign
                    "name"
                    ( Constant 1.0 )
                )
            parseStmt "  1   " `shouldBe` Right
                ( Out
                    ( Constant 1.0 )
                )

    describe "expr" $ do
        it "should parse numbers" $ do
            parseExpr "123" `shouldBe` Right
                ( Constant 123.0 )
        it "should parse float numbers" $ do
            parseExpr "123.456" `shouldBe` Right
                ( Constant 123.456 )
        it "should parse names" $ do
            parseExpr "name" `shouldBe` Right
                ( Symbol "name" )
        it "should parse additive expressions" $ do
            parseExpr "2+3" `shouldBe` Right
                ( BinaryOp Add
                    ( Constant 2.0 )
                    ( Constant 3.0 )
                )
            parseExpr "2-3" `shouldBe` Right
                ( BinaryOp Sub
                    ( Constant 2.0 )
                    ( Constant 3.0 )
                )
        it "should parse multiplicative expressions" $ do
            parseExpr "2*3" `shouldBe` Right
                ( BinaryOp Mul
                    ( Constant 2.0 )
                    ( Constant 3.0 )
                )
            parseExpr "2/3" `shouldBe` Right
                ( BinaryOp Div
                    ( Constant 2.0 )
                    ( Constant 3.0 )
                )
        it "should parse mixed expressions" $ do
            parseExpr "2+3*name-5/6" `shouldBe` Right
                ( BinaryOp Sub
                    ( BinaryOp Add
                        ( Constant 2.0 )
                        ( BinaryOp Mul
                            ( Constant 3.0 )
                            ( Symbol "name" )
                        )
                    )
                    ( BinaryOp Div
                        ( Constant 5.0 )
                        ( Constant 6.0 )
                    )
                )
        it "should parse mixed expressions with parens" $ do
            parseExpr "(2+3)*(name-5)/6" `shouldBe` Right
                ( BinaryOp Div
                    ( BinaryOp Mul
                        ( BinaryOp Add
                            ( Constant 2.0 )
                            ( Constant 3.0 )
                        )
                        ( BinaryOp Sub
                            ( Symbol "name" )
                            ( Constant 5.0 )
                        )
                    )
                    ( Constant 6.0 )
                )
        it "should parse negative numbers" $ do
            parseExpr "-2" `shouldBe` Right
                ( UnaryOp Negate
                    ( Constant 2.0 )
                )
        it "should parse expressions with negate" $ do
            parseExpr "2+-(-3*4)" `shouldBe` Right
                ( BinaryOp Add
                    ( Constant 2.0 )
                    ( UnaryOp Negate
                        ( BinaryOp Mul
                            ( UnaryOp Negate
                                ( Constant 3.0 )
                            )
                            ( Constant 4.0 )
                        )
                    )
                )
        it "should parse expressions with negate and subtraction" $ do
            parseExpr "2--3" `shouldBe` Right
                ( BinaryOp Sub
                    ( Constant 2.0 )
                    ( UnaryOp Negate
                        ( Constant 3.0 )
                    )
                )
        it "should return error on invalid input" $ do
            parseExpr "I`N`V`A`L`I`D" `shouldSatisfy` hasErrorOnPos 2
        it "should handle multiple negations" $ do
            parseExpr "2*--(3+4)" `shouldBe` Right
                ( BinaryOp Mul
                    ( Constant 2.0 )
                    ( UnaryOp Negate
                        ( UnaryOp Negate
                            ( BinaryOp Add
                                ( Constant 3.0 )
                                ( Constant 4.0 )
                            )
                        )
                    )
                )
        it "should skip spaces" $ do
            parseExpr "   2   " `shouldBe` Right
                ( Constant 2.0 )
            parseExpr " 2  +   -  (  3   *   4 )    " `shouldBe` Right
                ( BinaryOp Add
                    ( Constant 2.0 )
                    ( UnaryOp Negate
                        ( BinaryOp Mul
                            ( Constant 3.0 )
                            ( Constant 4.0 )
                        )
                    )
                )
        it "should parse balanced parens" $ do
            parseExpr "(((((3)))))" `shouldBe` Right
                ( Constant 3.0 )
        it "should return error on unbalanced parens" $ do
            parseExpr "(((((3))))" `shouldSatisfy` hasErrorOnPos 11
            parseExpr "((((3)))))" `shouldSatisfy` hasErrorOnPos 10
        it "should return error on empty input" $ do
            parseExpr "" `shouldSatisfy` hasErrorOnPos 1
        it "should return error on incomplete input" $ do
            parseExpr "1+2*" `shouldSatisfy` hasErrorOnPos 5


