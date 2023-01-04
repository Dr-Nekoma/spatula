{-# LANGUAGE DeriveGeneric #-}
module Types ( 
  typeSubstitution,
  extractName,
  Curryable(..), 
  Kind(..), 
  Type(..), 
  AbstractionInfo(..), 
  TListInfo(..),
  TVariableInfo(..),
  Literal(..),
  LetSort(..),
  Operator(..),
  Expression(..),
  Declaration(..),
  Label,
  extractName,
  Field) where

import Data.Text.Arbitrary ( Text )
import Data.Text ( unpack )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary(arbitrary), listOf )
import Test.QuickCheck.Arbitrary.ADT
    ( ToADTArbitrary, genericArbitrary )
import Text.Printf ( printf )
import System.Random
import Data.Bifunctor(Bifunctor(second))


refresh :: TVariableInfo -> TVariableInfo
refresh = id

abstractionSubstitution :: TVariableInfo -> Type -> AbstractionInfo -> AbstractionInfo
abstractionSubstitution from to abstraction@(AbstractionInfo label kind type') = 
  if label == from then abstraction else AbstractionInfo newLabel kind (typeSubstitution from to newBody)
  where newLabel = refresh label
        newBody = typeSubstitution label (TVariable newLabel) type'

typeSubstitution :: TVariableInfo -> Type -> Type -> Type
typeSubstitution placeHolder type' target =
  case target of
    TArrow parameter returnType ->
      TArrow
      (typeSubstitution placeHolder type' parameter)
      (typeSubstitution placeHolder type' returnType)
    TApplication abstractionType argumentType ->
      TApplication
        (typeSubstitution placeHolder type' abstractionType)
        (typeSubstitution placeHolder type' argumentType)
    TAbstraction info -> TAbstraction (abstractionSubstitution placeHolder type' info)
    TForall info -> TForall (abstractionSubstitution placeHolder type' info) 
    TVariable identifier | identifier == placeHolder -> type'
                         | otherwise -> TVariable identifier
    TUnit -> TUnit
    TInteger -> TInteger
    TRational -> TRational
    TBool -> TBool
    TString -> TString
    TList (TListInfo listType) -> TList . TListInfo $ fmap (typeSubstitution placeHolder type') listType
    TAnonymousRecord fields -> TAnonymousRecord $ fmap (second $ typeSubstitution placeHolder type') fields
    TAlias name type'' -> TAlias name $ typeSubstitution placeHolder type'' type'
    
class Curryable a where  
    kurry :: a -> a -> a

data Kind =
    StarK
  | ArrowK Kind Kind
  deriving (Eq, Generic, Ord)

instance Show Kind where
  show StarK = "*"
  show (ArrowK kind1 kind2) = show kind1 ++ " -> " ++ show kind2

instance Curryable Kind where
  kurry = ArrowK

instance Arbitrary Kind where
  arbitrary = genericArbitrary

data AbstractionInfo = AbstractionInfo TVariableInfo Kind Type
  deriving (Generic, Ord)

instance Show AbstractionInfo where
  show (AbstractionInfo label kind type') = printf "%s. %s; %s" (unpack $ extractName label) (show kind) (show type')

instance Arbitrary AbstractionInfo where
  arbitrary = genericArbitrary

instance ToADTArbitrary AbstractionInfo

instance Eq AbstractionInfo where
  (AbstractionInfo ident1 kind1 type1) == (AbstractionInfo ident2 kind2 type2) =
    if kind1 == kind2 then
      if ident1 == ident2
      then type1 == type2
      else let newLabel = refresh ident1 
           in typeSubstitution ident1 (TVariable newLabel) type1 == typeSubstitution ident2 (TVariable newLabel) type2
    else False

-- TODO Test if you can pass the type of the list to something
-- When type aliases are added we should include type alias for list that returns the List|T| type
-- Do we still need the maybe type here? Consider that we are implementing the type list using type alias
data TListInfo = TListInfo (Maybe Type)
  deriving (Generic, Ord)

instance Arbitrary TListInfo where
  arbitrary = genericArbitrary

instance ToADTArbitrary TListInfo

instance Eq TListInfo where
  (TListInfo Nothing) == _ = True
  _ == (TListInfo Nothing) = True
  (TListInfo (Just x)) == (TListInfo (Just y)) = x == y

data TVariableInfo = Name Text | NameId (Text, Int) deriving (Eq, Generic)

extractName :: TVariableInfo -> Text
extractName (Name name) = name
extractName (NameId (name, _)) = name

instance Ord TVariableInfo where
  compare (Name label1) (Name label2) = compare label1 label2
  compare (Name label1) (NameId (label2, _)) = compare label1 label2
  compare (NameId (label1, _)) (Name label2) = compare label1 label2
  compare (NameId (label1, iD1)) (NameId (label2, iD2)) = if iD1 == iD2 then EQ else compare label1 label2

instance Show TVariableInfo where
  show (Name name) = show name
  show (NameId (name, iD)) = show name ++ " - ID: " ++ show iD

instance Arbitrary TVariableInfo where
  arbitrary = genericArbitrary

instance ToADTArbitrary TVariableInfo

data Type
    = TUnit
    | TInteger
    | TRational
    | TBool
    | TString
    | TList TListInfo
    | TAnonymousRecord [(Text, Type)]
    | TArrow Type Type
    | TVariable TVariableInfo
    | TForall AbstractionInfo
    | TApplication Type Type
    | TAbstraction AbstractionInfo
    | TAlias Text Type
    | TAliasPlaceholder Text
    | TAlgebraic [(Text, [Type])]
    deriving (Generic, Eq, Ord)

instance Show Type where
  show TUnit = "Unit"
  show TInteger = "Integer"
  show TRational = "Rational"
  show TBool = "Bool"
  show TString = "String"
  show (TList (TListInfo (Just type'))) = printf "List|%s|" (show type')
  show (TList (TListInfo Nothing)) = "List|_|"
  show (TArrow source target) =
    case source of
      TArrow _ _ -> printf "(%s) -> %s" (show source) (show target)
      TForall _ -> printf "(%s) -> %s" (show source) (show target)
      _ ->  printf "%s -> %s" (show source) (show target)
  show (TVariable label) = unpack $ extractName label
  show (TForall info) = "forall " ++ show info
  show (TApplication fun arg) = printf "%s <- %s" (show fun) (show arg)
  show (TAbstraction (AbstractionInfo label kind type')) = printf "lambda %s : %s -> %s" (unpack $ extractName label) (show kind) (show type')
  show (TAlias name type') = "(Alias " ++ unpack name ++ " , " ++ show type' ++ ")"
  show (TAliasPlaceholder name) = unpack name
  show (TAnonymousRecord []) = printf "| Anonymous Record | EMPTY"
  show (TAnonymousRecord list) = go "| Anonymous Record | " list
    where go acc [] = acc ++ "\n"
          go acc ((name, type'):xs) = go (acc ++ "Field: " ++ show name ++ " - Type: " ++ show type' ++ " ") xs

instance Curryable Type where
  kurry = TArrow

instance Arbitrary Type where
  arbitrary = genericArbitrary

instance ToADTArbitrary Type

data Literal
    = LUnit
    | LInteger Integer
    | LRational Rational
    | LBool Bool
    | LString Text
--    | LTuple [Expression]
    deriving (Generic, Eq)

instance Arbitrary Literal where
  arbitrary = genericArbitrary

instance ToADTArbitrary Literal

instance Show Literal where
  show LUnit = "()"
  show (LInteger int) = show int
  show (LRational rational) = show rational
  show (LBool bool) = show bool
  show (LString string) = "\"" ++ unpack string ++ "\""

data LetSort = In | Plus
  deriving (Generic, Eq, Show)

data Operator = OpConcat | OpPlus | OpMinus | OpDiv | OpMul | OpAnd | OpOr | OpEqual | OpLessThan
  deriving (Generic, Eq, Enum, Bounded)

instance Show Operator where
  show OpPlus  = "+"
  show OpMinus = "-"
  show OpMul   = "*"
  show OpDiv   = "/"
  show OpAnd   = "and"
  show OpOr    = "or"
  show OpEqual = "="
  show OpConcat = "^"
  show OpLessThan = "<"

instance Arbitrary LetSort where
  arbitrary = genericArbitrary

instance Arbitrary Operator where
  arbitrary = genericArbitrary

data Declaration
    = DeclExpr Expression
    | DeclFun Text Type Expression
    | DeclVal Text Expression
    | DeclType Text Type
    deriving (Generic, Eq)

instance Arbitrary Declaration where
  arbitrary = genericArbitrary

instance Show Declaration where
  show (DeclExpr expr) = "Expression: " ++ show expr
  show (DeclFun name type' expr) = "Function: " ++ unpack name ++ " : " ++ show type' ++ " = " ++ show expr
  show (DeclVal name literal) = "Value: " ++ unpack name ++ " = " ++ show literal
  show (DeclType name type') = "Type: " ++ unpack name ++ " = " ++ show type'

type Label = Text
type Field = (Label, Expression)

data Expression
    = ELiteral Literal
    | EVariable Text
    | EOperation Operator [Expression]
    | ELet LetSort [(Text, Expression)] Expression -- What about kind checking this?
    | EAbstraction Text Type (Maybe Type) Expression
    | EApplication Expression Expression
    | ECondition Expression Expression Expression
    | ETypeAbstraction TVariableInfo Kind (Maybe Type) Expression
    | ETypeApplication Expression Type
    | EList [Expression]
    | EAnonymousRecord [Field]
    | EProgn [Expression]
    | ERecordProjection Expression Label
    | ERecordUpdate Expression [(Label, Expression)]
    deriving (Generic, Eq, Show)

instance Arbitrary Expression where
  arbitrary = genericArbitrary
