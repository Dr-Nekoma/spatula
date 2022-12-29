{-# LANGUAGE OverloadedStrings #-}

module Compilation.Emit (compile, generateIL) where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text, concat, pack, unpack)
import Text.Printf (printf)
import Types
  ( AbstractionInfo (AbstractionInfo),
    Declaration (..),
    Expression (..),
    Kind (..),
    LetSort (..),
    Literal (LBool, LInteger, LRational, LString, LUnit),
    Operator (..),
    TListInfo (..),
    TVariableInfo (..),
    Type (..),
  )

textify :: Show a => a -> Text
textify = pack . show

interopNETCallables :: Map.Map Text Text
interopNETCallables =
  Map.fromList $
    [("print", "call void [mscorlib]System.Console::WriteLine(class System.Object)")]

translate :: Expression -> Text
translate (ELiteral literal) =
  case literal of
    LString value -> "ldstr " <> textify value <> "\n"
    LInteger value -> "ldc.i4 \"" <> textify value <> "\"\n"
    _ -> undefined
translate (EVariable name) =
  fromMaybe
    (error "Could not find variable in definitions for interop.")
    (Map.lookup name interopNETCallables)
-- TODO :: Fold the curried abstractions to a single one
translate (EAbstraction var argType returnType body) =
  case body of
    EAbstraction _ _ _ _ -> error "Think of how we can defined nested methods if we go that route"
    _ -> translate body
translate (EApplication abs arg) =
  let emittedArg = translate arg
      emittedFun = translate abs
   in emittedArg <> emittedFun

----------------------------------------------------------

generateIL :: [Declaration] -> [Text] -> [Text]
generateIL [] acc =
  acc
generateIL (declr : rest) acc =
  let buildMethod name body = pack $ printf ".method static public void %s() cil managed {%s\nret\n}" (unpack name) (unpack body)
      embedMetadata body = pack $ printf "\n.entrypoint\n.maxstack 10\n%s" (unpack body)
   in case declr of
        DeclExpr expr ->
          let il = translate expr
           in generateIL rest (il : acc)
        DeclFun "main" resType' abs ->
          let entryPoint = (buildMethod "main" . embedMetadata . translate) abs
           in acc <> [entryPoint]
        DeclFun _ _ _ -> undefined

compile :: [Declaration] -> Text
compile program =
  let moduleMatch (DeclModule _ _) = True
      moduleMatch _ = False
   in case find moduleMatch program of
        Just (DeclModule name scope) ->
          let emittedScope = Data.Text.concat $ generateIL scope []
           in (pack $ printf ".assembly extern mscorlib {}\n.assembly %s {}" name)
                <> ".class public abstract sealed auto ansi "
                <> name
                <> (pack $ printf " extends [System.Runtime]System.Object {\n%s}" emittedScope)
        Just _ -> error "Impossible."
        Nothing -> error "A module declaration is required."
