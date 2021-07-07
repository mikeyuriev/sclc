module ContextSpec where

import Test.Hspec

import SmallCalc.Context
import SmallCalc.Types

spec :: Spec
spec = do
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
