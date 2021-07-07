import Data.Either (isLeft)
import Text.Printf (printf)

import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.Pos

import Test.Hspec

import SmallCalc.Context
import SmallCalc.Types
import SmallCalc.Eval
import SmallCalc.Parser.Internal

main :: IO ()
main = hspec $ do
    describe "Parser" $ do
        parserSpec
    describe "Eval" $ do
        evalSpec
    describe "Symbols" $ do
        contextSpec

parseExpr :: String -> Either ParseError Expr
parseExpr = parse (expr <* end) ""

parseStmt :: String -> Either ParseError Statement
parseStmt = parse (statement <* end) ""

hasErrorOnPos :: Int -> Either ParseError a -> Bool
hasErrorOnPos x (Left err) = x == (sourceColumn . errorPos $ err)
hasErrorOnPos _ _          = False

contextSpec :: Spec
contextSpec = do
    describe "getSymbol" $ do
        let
            emptySyms = Context [] undefined
            syms1 = Context [("name", 0.0)] undefined
            syms2 = Context [("name0", 0.0), ("name1", 1.0), ("name2", 2.0)] undefined
        it "should find value" $ do
            getSymbol syms1 "name" `shouldBe` Right 0.0
            getSymbol syms2 "name0" `shouldBe` Right 0.0
            getSymbol syms2 "name1" `shouldBe` Right 1.0
            getSymbol syms2 "name2" `shouldBe` Right 2.0
        it "should return error if no value in table" $ do
            getSymbol emptySyms "name" `shouldBe`
                Left (EUndefined "name")
            getSymbol syms2 "name" `shouldBe`
                Left (EUndefined "name")
    describe "put" $ do
        it "should add value" $ do
            putSymbol
                ( Context [] 0.0 ) "name" 0.0
                `shouldBe`
                Context [("name", 0.0)] 0.0
            putSymbol
                ( Context [("name0", 0.0)] 0.0 ) "name1" 1.0
                `shouldBe`
                Context [("name0", 0.0), ("name1", 1.0)] 1.0
        it "should update value" $ do
            putSymbol
                ( Context [("name", 0.0)] 0 ) "name" 1.0
                `shouldBe`
                Context [("name", 1.0)] 1.0

parserSpec :: Spec
parserSpec = do
    describe "parseStatement" $ do
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

    describe "parseExpr" $ do
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

evalSpec :: Spec
evalSpec = do
    describe "evalExpr" $ do
        it "should evaluate expression" $ do
            printf "%.2f" <$> evalExpr (Context [("name", 4.3)] undefined)
                ( BinaryOp Mod
                    ( BinaryOp Mul
                        ( BinaryOp Add
                            ( Constant 2.1 )
                            ( Constant 3.2)
                        )
                        ( UnaryOp Negate
                            ( Symbol "name" )
                        )
                    )
                    ( Constant 5.4 )
                ) `shouldBe` Right "4.21"
        it "should return error on division by zero" $ do
            evalExpr (Context [] undefined)
                ( BinaryOp Div
                    ( Constant 1.0 )
                    ( BinaryOp Sub
                        ( Constant 2.0 )
                        ( Constant 2.0 )
                    )
                ) `shouldBe` Left EDivisionByZero
            evalExpr (Context [] undefined)
                ( BinaryOp Mod
                    ( Constant 0.0 )
                    ( Constant 0.0 )
                ) `shouldBe` Left EDivisionByZero
        it "should return error on undefined symbols" $ do
            evalExpr
                ( Context [ ("name", 0.0) ] undefined )
                ( Symbol "undefined" )
            `shouldBe` Left (EUndefined "undefined")

    describe "execStatement" $ do
        describe "Out" $ do
            it "should return execution result" $ do
                execStatement
                    ( Context [] 0 )
                    ( Out (Constant 1.0) )
                `shouldBe` Right (Context [] 1.0)
            it "shouldn't change context" $ do
                execStatement
                    ( Context [("name", 1.0)] 1.0 )
                    ( Out (Constant 1.0))
                `shouldBe` Right (Context [("name", 1.0)] 1.0)
        describe "Assign" $ do
            it "should add data to context and return execution result" $ do
                execStatement
                    ( Context [("name0", 0.0)] 0.0)
                    ( Assign "name1" (Constant 1.0) )
                `shouldBe` Right
                    ( Context [ ("name0", 0.0), ("name1", 1.0) ] 1.0 )
            it "should replace data in context and return execution result" $ do
                execStatement
                    ( Context [("name", 0.0)] 0.0)
                    ( Assign "name" (Constant 1.0) )
                `shouldBe` Right
                    ( Context [ ("name", 1.0) ] 1.0 )

