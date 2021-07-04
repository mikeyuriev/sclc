import Data.Either (isLeft)
import Text.Printf (printf)
import Test.Hspec

import SmallCalc.AST
import SmallCalc.Parser
import SmallCalc.Eval
import SmallCalc.Error

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        parserSpec
    describe "Eval" $ do
        evalSpec

parserSpec :: Spec
parserSpec = do
    describe "parseLine" $ do
        it "should parse integer numbers" $ do
            parseLine "123" `shouldBe` Right
                ( Constant 123.0 )
        it "should parse float numbers" $ do
            parseLine "123.456" `shouldBe` Right
                ( Constant 123.456 )
        it "should parse additive expressions" $ do
            parseLine "2+3" `shouldBe` Right
                ( BinaryOp Add
                    ( Constant 2.0 )
                    ( Constant 3.0 )
                )
            parseLine "2-3" `shouldBe` Right
                ( BinaryOp Sub
                    ( Constant 2.0 )
                    ( Constant 3.0 )
                )
        it "should parse multiplicative expressions" $ do
            parseLine "2*3" `shouldBe` Right
                ( BinaryOp Mul
                    ( Constant 2.0 )
                    ( Constant 3.0 )
                )
            parseLine "2/3" `shouldBe` Right
                ( BinaryOp Div
                    ( Constant 2.0 )
                    ( Constant 3.0 )
                )
        it "should parse mixed expressions" $ do
            parseLine "2+3*4-5/6" `shouldBe` Right
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
            parseLine "(2+3)*(4-5)/6" `shouldBe` Right
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
            parseLine "-2" `shouldBe` Right
                ( UnaryOp Negate
                    ( Constant 2.0 )
                )
        it "should parse expressions with negate" $ do
            parseLine "2+-(-3*4)" `shouldBe` Right
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
            parseLine "2--3" `shouldBe` Right
                ( BinaryOp Sub
                    ( Constant 2.0 )
                    ( UnaryOp Negate
                        ( Constant 3.0 )
                    )
                )
        it "should return error on invalid input" $ do
            parseLine "I`N`V`A`L`I`D" `shouldSatisfy` isLeft
        it "should handle multiple negations" $ do
            parseLine "2*--(3+4)" `shouldBe` Right
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
            printf "%.2f" <$> eval
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
                ) `shouldBe` Right "4.21"
        it "should return error on division by zero" $ do
            eval
                ( BinaryOp Div
                    ( Constant 1.0 )
                    ( BinaryOp Sub
                        ( Constant 2.0 )
                        ( Constant 2.0 )
                    )
                ) `shouldBe` Left DivisionByZero
            eval
                ( BinaryOp Mod
                    ( Constant 0.0 )
                    ( Constant 0.0 )
                ) `shouldBe` Left DivisionByZero
