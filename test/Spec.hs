import Data.Either (isLeft)
import Text.Printf (printf)
import Test.Hspec
import qualified Text.Parsec as P

import SmallCalc.AST
import SmallCalc.Parser
import SmallCalc.Eval

parseExpr :: String -> Either P.ParseError Node
parseExpr = P.parse expr ""

parseLine :: String -> Either P.ParseError Node
parseLine = P.parse line ""

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        parserSpec
    describe "Eval" $ do
        evalSpec

parserSpec :: Spec
parserSpec = do
    describe "expr" $ do
        it "should parse integer numbers" $ do
            parseExpr "123" `shouldBe` Right
                ( Constant 123.0 )
        it "should parse float numbers" $ do
            parseExpr "123.456" `shouldBe` Right
                ( Constant 123.456 )
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
            parseExpr "2+3*4-5/6" `shouldBe` Right
                ( BinaryOp Sub
                    ( BinaryOp Add
                        ( Constant 2.0 )
                        ( BinaryOp Mul
                            ( Constant 3.0 )
                            ( Constant 4.0 )
                        )
                    )
                    ( BinaryOp Div
                        ( Constant 5.0 )
                        ( Constant 6.0 )
                    )
                )
        it "should parse mixed expressions with parens" $ do
            parseExpr "(2+3)*(4-5)/6" `shouldBe` Right
                ( BinaryOp Div
                    ( BinaryOp Mul
                        ( BinaryOp Add
                            ( Constant 2.0 )
                            ( Constant 3.0 )
                        )
                        ( BinaryOp Sub
                            ( Constant 4.0 )
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
            parseExpr "I`N`V`A`L`I`D" `shouldSatisfy` isLeft
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

    describe "line" $ do
        it "should skip spaces" $ do
            parseLine "   2   " `shouldBe` Right
                ( Constant 2.0 )
            parseLine " 2  +   -  (  3   *   4 )    " `shouldBe` Right
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
            parseLine "(((((3)))))" `shouldBe` Right
                ( Constant 3.0 )
        it "should return error on unbalanced parens" $ do
            parseLine "(((((3))))" `shouldSatisfy` isLeft
            parseLine "((((3)))))" `shouldSatisfy` isLeft
        it "should return error on empty input" $ do
            parseLine "" `shouldSatisfy` isLeft
        it "should return error on incomplete input" $ do
            parseLine "1+2*" `shouldSatisfy` isLeft

evalSpec :: Spec
evalSpec = do
    describe "eval" $ do
        it "should evaluate node" $ do
            printf "%.2f" (
                eval
                    ( BinaryOp Mod
                        ( BinaryOp Mul
                            ( BinaryOp Add
                                ( Constant 2.1 )
                                ( Constant 3.2)
                            )
                            ( UnaryOp Negate
                                ( Constant 4.3 )
                            )
                        )
                        ( Constant 5.4 )
                    )
                ) `shouldBe` "4.21"
