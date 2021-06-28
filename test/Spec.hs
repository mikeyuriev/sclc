import Data.Either (isLeft)
import Test.Hspec
import Text.Parsec

import SmallCalc.AST
import SmallCalc.Parser
import SmallCalc.Eval

parseExpr :: String -> Either ParseError Node
parseExpr = parse expr ""

parseLine :: String -> Either ParseError Node
parseLine = parse line ""

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
                ( Value $ Constant 123.0 )
        it "should parse float numbers" $ do
            parseExpr "123.456" `shouldBe` Right
                ( Value $ Constant 123.456 )
        it "should parse additive expressions" $ do
            parseExpr "2+3" `shouldBe` Right
                ( BinaryOp Add
                    ( Value $ Constant 2.0 )
                    ( Value $ Constant 3.0 )
                )
        it "should parse multiplicative expressions" $ do
            parseExpr "2*3" `shouldBe` Right
                ( BinaryOp Mul
                    ( Value $ Constant 2.0 )
                    ( Value $ Constant 3.0 )
                )
        it "should parse mixed expressions" $ do

            parseExpr "2+3*4" `shouldBe` Right
                ( BinaryOp Add
                    ( Value $ Constant 2.0 )
                    ( BinaryOp Mul
                        ( Value $ Constant 3.0 )
                        ( Value $ Constant 4.0 )
                    )
                )
        it "should parse mixed expressions with parens" $ do
            parseExpr "(2+3)*4" `shouldBe` Right
                ( BinaryOp Mul
                    ( BinaryOp Add
                        ( Value $ Constant 2.0 )
                        ( Value $ Constant 3.0 )
                    )
                    ( Value $ Constant 4.0 )
                )
        it "should parse negative numbers" $ do
            parseExpr "-2" `shouldBe` Right
                ( UnaryOp Negate
                    ( Value $ Constant 2.0 )
                )
        it "should parse expressions with negate" $ do
            parseExpr "2+-(3*4)" `shouldBe` Right
                ( BinaryOp Add
                    ( Value $ Constant 2.0 )
                    ( UnaryOp Negate
                        ( BinaryOp Mul
                            ( Value $ Constant 3.0 )
                            ( Value $ Constant 4.0 )
                        )
                    )
                )
        it "should return error on invalid input" $ do
            parseExpr "I`N`V`A`L`I`D" `shouldSatisfy` isLeft

    describe "line" $ do
        it "should parse balanced parens" $ do
            parseLine "(((((3)))))" `shouldBe` Right
                ( Value $ Constant 3.0 )
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
            eval
                ( BinaryOp Mul
                    ( BinaryOp Add
                        ( Value $ Constant 2.0 )
                        ( Value $ Constant 3.0 )
                    )
                    ( UnaryOp Negate
                        ( Value $ Constant 4.0 )
                    )
                )
            `shouldBe` -20.0
