{-# LANGUAGE OverloadedStrings #-}
module TestTyper ( typeCheckingTest ) where

import Test.Hspec
import Typer
import Types

typeCheckingTest :: SpecWith ()
typeCheckingTest = do
  describe "Test type checking" $ do
  --  describe "Literals" $ do
      
      -- it "Boolean" $ do
      --    typeCheck (ELiteral (LBool True)) `shouldBe` Right TBool

      -- it "Integer" $ do
      --    property $ \x y -> typeCheck (ELiteral (x :: Literal)) == Right (y :: Type)

    describe "System F" $ do
      let identity = ETypeAbstraction "type" (EAbstraction "value" (TVariable "type") Nothing (EVariable "value"))
          body     = EApplication (EVariable "fun") (EVariable "value")
          abst     = ETypeAbstraction "typeA" (ETypeAbstraction "typeB" (EAbstraction "fun" (TArrow (TVariable "typeA") (TVariable "typeB")) Nothing (EAbstraction "value" (TVariable "typeA") Nothing body)))
      it "forall a. (x:a) -> x" $ do
        typeCheck identity
          `shouldBe`
          Right (TForall (AbstractionInfo "type" (TArrow (TVariable "type") (TVariable "type"))))
      it "forall a. (a:a) -> (a:a)" $ do
        typeCheck (ETypeAbstraction "type" (EAbstraction "type" (TVariable "type") Nothing (EVariable "type")))
          `shouldBe`
          Right (TForall (AbstractionInfo "type" (TArrow (TVariable "type") (TVariable "type"))))
      it "forall a. a -> a $ Integer " $ do
        typeCheck (ETypeApplication identity TInteger)
          `shouldBe`
          Right (TArrow TInteger TInteger)
      it "forall a. a -> a $ Integer $ 2" $ do
        typeCheck (EApplication (ETypeApplication (ETypeAbstraction "type" (EAbstraction "value" (TVariable "type") Nothing (EVariable "value"))) TInteger) (ELiteral (LInteger 2)))
          `shouldBe`
          Right TInteger
      it "forall a. b. (a -> b) -> a -> b" $ do
        let type' = TForall (AbstractionInfo "typeA" (TForall (AbstractionInfo "typeB" (TArrow (TArrow (TVariable "typeA") (TVariable "typeB")) (TArrow (TVariable "typeA") (TVariable "typeB"))))))
        typeCheck abst `shouldBe` Right type'
