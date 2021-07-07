module EvalSpec where

import Test.Hspec
import Text.Printf

import SmallCalc.Eval
import SmallCalc.Types

spec :: Spec
spec = do
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


