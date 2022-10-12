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
      let identity = ETypeAbstraction "type" (EAbstraction "value" (TVariable "type") (EVariable "value"))
          body     = EApplication (EVariable "fun") (EVariable "value")
          abst     = ETypeAbstraction "typeA" (ETypeAbstraction "typeB" (EAbstraction "fun" (TArrow (TVariable "typeA") (TVariable "typeB")) (EAbstraction "value" (TVariable "typeA") body)))
      it "forall a. (x:a) -> x" $ do
        typeCheck identity
          `shouldBe`
          Right (TForall (TForallInfo "type" (TArrow (TVariable "type") (TVariable "type"))))
      it "forall a. (a:a) -> (a:a)" $ do
        typeCheck (ETypeAbstraction "type" (EAbstraction "type" (TVariable "type") (EVariable "type")))
          `shouldBe`
          Right (TForall (TForallInfo "type" (TArrow (TVariable "type") (TVariable "type"))))
      it "forall a. a -> a $ Integer " $ do
        typeCheck (ETypeApplication identity TInteger)
          `shouldBe`
          Right (TArrow TInteger TInteger)
      it "forall a. a -> a $ Integer $ 2" $ do
        typeCheck (EApplication (ETypeApplication (ETypeAbstraction "type" (EAbstraction "value" (TVariable "type") (EVariable "value"))) TInteger) (ELiteral (LInteger 2)))
          `shouldBe`
          Right TInteger
      it "forall a. b. (a -> b) -> a -> b" $ do
        let type' = TForall (TForallInfo "typeA" (TForall (TForallInfo "typeB" (TArrow (TArrow (TVariable "typeA") (TVariable "typeB")) (TArrow (TVariable "typeA") (TVariable "typeB"))))))
        typeCheck abst `shouldBe` Right type'
