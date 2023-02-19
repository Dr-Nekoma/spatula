{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Types ( 
  typeSubstitution,
  extractName,
  Curryable(..), 
  Kind(..),
  Kind'(..),
  Type(..),
  Type'(..),
  AbstractionInfo(..),
  AbstractionInfo'(..), 
  TListInfo(..),
  TListInfo'(..),
  TVariableInfo(..),
  TVariableInfo'(..),  
  Literal(..),
  Literal'(..),
  LetSort(..),
  LetSort'(..),
  Operator(..),
  Operator'(..),
  Pattern(..),
  Pattern'(..),
  Expression(..),
  Expression'(..),
  Declaration(..),
  Declaration'(..),  
  Label,
  extractName,
  FullNode(..),
  Metadata(..),
  removeMetadata,
  getMetadata,
  makeEmptyNode,
  makeNamedMetadata,
  Field) where

import Data.Text.Arbitrary ( Text )
import Data.Text ( unpack, append )
import Text.Printf ( printf )
import System.Random
import Data.Bifunctor(Bifunctor(second))
import Text.Parsec
import Text.Parsec.Pos (initialPos)

refresh :: TVariableInfo -> TVariableInfo
refresh = id

abstractionSubstitution :: TVariableInfo -> Type -> AbstractionInfo -> AbstractionInfo
abstractionSubstitution from to abstraction@(FullNode meta (AbstractionInfo' label kind type')) = 
  if label == from then abstraction else FullNode meta $ AbstractionInfo' newLabel kind (typeSubstitution from to newBody)
  where newLabel = refresh label
        newBody = typeSubstitution label (FullNode meta (TVariable newLabel)) type'

typeSubstitution :: TVariableInfo -> Type -> Type -> Type
typeSubstitution placeHolder type' target' = func <$> target'
  where func target = case target of
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
                        TVariable identifier | identifier == placeHolder -> removeMetadata type'
                                             | otherwise -> TVariable identifier
                        TUnit -> TUnit
                        TInteger -> TInteger
                        TRational -> TRational
                        TBool -> TBool
                        TString -> TString
                        TList info -> TList $ fmap (\(TListInfo' x) -> TListInfo' $ fmap (typeSubstitution placeHolder type') x) info
                        TAnonymousRecord fields -> TAnonymousRecord $ fmap (second (typeSubstitution placeHolder type')) fields
                        TNominalRecord name fields -> TNominalRecord name $ fmap (second $ typeSubstitution placeHolder type') fields
                        TAlias name type'' -> TAlias name $ typeSubstitution placeHolder type'' type'
                        TAliasPlaceholder name -> TAliasPlaceholder name
                        TAlgebraic fields -> TAlgebraic $ fmap (second (fmap (typeSubstitution placeHolder type'))) fields
    
class Curryable a b where  
    kurry :: a -> a -> b

type Kind = FullNode Kind'

data Kind' =
    StarK
  | ArrowK Kind Kind
  deriving (Eq, Ord)

instance Show Kind' where
  show StarK = "Star"
  show (ArrowK kind1 kind2) =
    case removeMetadata kind1 of
      ArrowK _ _ -> printf "(%s) -> %s" (show kind1) (show kind2)
      _ ->  printf "%s -> %s" (show kind1) (show kind2)

instance Curryable Kind Kind' where
  kurry = ArrowK

instance Curryable Kind Kind where
  kurry k1@(FullNode m1 _) k2@(FullNode _ _)= FullNode m1 $ kurry k1 k2

type AbstractionInfo = FullNode AbstractionInfo' 

data AbstractionInfo' = AbstractionInfo' TVariableInfo Kind Type
  deriving Ord

instance Show AbstractionInfo' where
  show (AbstractionInfo' label kind type') = printf "%s. %s; %s" (unpack $ extractName (removeMetadata label)) (show kind) (show type')

instance Eq AbstractionInfo' where
  (AbstractionInfo' ident1@(FullNode meta1 _) kind1 type1) == (AbstractionInfo' ident2@(FullNode meta2 _) kind2 type2) =
    if kind1 == kind2 then
      if ident1 == ident2
      then type1 == type2
      else let newLabel = refresh ident1 
           in typeSubstitution ident1 (FullNode meta1 $ TVariable newLabel) type1 == typeSubstitution ident2 (FullNode meta2 $ TVariable newLabel) type2
    else False

-- TODO Test if you can pass the type of the list to something
-- When type aliases are added we should include type alias for list that returns the List|T| type
-- Do we still need the maybe type here? Consider that we are implementing the type list using type alias
type TListInfo = FullNode TListInfo'

data TListInfo' = TListInfo' (Maybe Type)
  deriving Ord

instance Eq TListInfo' where
  (TListInfo' Nothing) == _ = True
  _ == (TListInfo' Nothing) = True
  (TListInfo' (Just x)) == (TListInfo' (Just y)) = x == y

type TVariableInfo = FullNode TVariableInfo' 

data TVariableInfo' = Name Text | NameId (Text, Int) deriving Eq

extractName :: TVariableInfo' -> Text
extractName (Name name) = name
extractName (NameId (name, _)) = name

instance Ord TVariableInfo' where
  compare (Name label1) (Name label2) = compare label1 label2
  compare (Name label1) (NameId (label2, _)) = compare label1 label2
  compare (NameId (label1, _)) (Name label2) = compare label1 label2
  compare (NameId (label1, iD1)) (NameId (label2, iD2)) = if iD1 == iD2 then EQ else compare label1 label2

instance Show TVariableInfo' where
  show (Name name) = show name
  show (NameId (name, iD)) = show name ++ " - ID: " ++ show iD

type Type = FullNode Type'

data Type'
    = TUnit
    | TInteger
    | TRational
    | TBool
    | TString
    | TList TListInfo
    | TAnonymousRecord [(FullNode Text, Type)]
    | TNominalRecord (FullNode Text) [(FullNode Text, Type)]
    | TArrow Type Type
    | TVariable TVariableInfo
    | TForall AbstractionInfo
    | TApplication Type Type
    | TAbstraction AbstractionInfo
    | TAlias (FullNode Text) Type
    | TAliasPlaceholder (FullNode Text)
    | TAlgebraic [(FullNode Text, [Type])]
    deriving (Eq, Ord)

instance Show Type' where
  show TUnit = "Unit"
  show TInteger = "Integer"
  show TRational = "Rational"
  show TBool = "Bool"
  show TString = "String"
  show (TList (FullNode _ (TListInfo' (Just type')))) = printf "|List %s|" (show type')
  show (TList (FullNode _ (TListInfo' Nothing))) = "|List|"
  show (TArrow source target) =
    case removeMetadata source of
      TArrow _ _ -> printf "(%s) -> %s" (show source) (show target)
      TForall _ -> printf "(%s) -> %s" (show source) (show target)
      _ ->  printf "%s -> %s" (show source) (show target)
  show (TVariable label) = unpack $ extractName (removeMetadata label)
  show (TForall info) = "forall " ++ show info
  show (TApplication fun arg) = printf "|%s %s|" (show fun) (show arg)
  show (TAbstraction (FullNode _ (AbstractionInfo' label kind type'))) = printf "lambda %s : %s -> %s" (unpack $ extractName (removeMetadata label)) (show kind) (show type')
  show (TAlias name _) = unpack (removeMetadata name)
  show (TAliasPlaceholder name) = unpack (removeMetadata name)
  show (TAnonymousRecord []) = printf "| Anonymous Record | EMPTY"
  show (TAnonymousRecord list) = go "| Anonymous Record | " list
    where go acc [] = acc ++ "\n"
          go acc ((name, type'):xs) = go (acc ++ "Field: " ++ show name ++ " - Type: " ++ show type' ++ " ") xs
  show (TNominalRecord name []) = unpack (removeMetadata name)
  show (TNominalRecord name list) = go ("| " ++ unpack (removeMetadata name) ++ " |") list
    where go acc [] = acc ++ "\n"
          go acc ((name, type'):xs) = go (acc ++ "Field: " ++ show name ++ " - Type: " ++ show type' ++ " ") xs
  show (TAlgebraic []) = "Empty ADT"
  show (TAlgebraic list) = go "| ADT | " list
    where go acc [] = acc ++ "\n"
          go acc ((name, types):xs) = go (acc ++ "Tag: " ++ show name ++ " - Types: " ++ show types ++ " ") xs
  
instance Curryable Type Type' where
  kurry = TArrow

instance Curryable Type Type where
  kurry t1@(FullNode m1 _) t2@(FullNode _ _)= FullNode m1 $ kurry t1 t2

type Literal = FullNode Literal'

data Literal'
    = LUnit
    | LInteger Integer
    | LRational Rational
    | LBool Bool
    | LString Text
--    | LTuple [Expression]
    deriving Eq

instance Show Literal' where
  show LUnit = "()"
  show (LInteger int) = show int
  show (LRational rational) = show rational
  show (LBool bool) = show bool
  show (LString string) = "\"" ++ unpack string ++ "\""

type LetSort = FullNode LetSort'

data LetSort' = In | Plus
  deriving (Eq, Show)

type Operator = FullNode Operator'

data Operator' = OpConcat | OpPlus | OpMinus | OpDiv | OpMul | OpAnd | OpOr | OpEqual | OpLessThan
  deriving (Eq, Enum, Bounded)

instance Show Operator' where
  show OpPlus  = "+"
  show OpMinus = "-"
  show OpMul   = "*"
  show OpDiv   = "/"
  show OpAnd   = "and"
  show OpOr    = "or"
  show OpEqual = "="
  show OpConcat = "^"
  show OpLessThan = "<"

type Declaration = FullNode Declaration'

data Declaration'
    = DeclExpr Expression
    | DeclFun (FullNode Text) Type Expression
    | DeclVal (FullNode Text) Expression
    | DeclType (FullNode Text) Type
    | DeclModule (FullNode Text) [Declaration]
    | DeclLoad (FullNode FilePath)
    deriving Eq

-- renameDeclaration :: Text -> Declaration -> Declaration
-- renameDeclaration _ (DeclExpr expr) = DeclExpr expr
-- renameDeclaration toAppend (DeclVal name value) = DeclVal (append toAppend name) value
-- renameDeclaration toAppend (DeclType name t) = DeclType (append toAppend name) t
-- renameDeclaration toAppend (DeclFun name t expr) = DeclFun (append toAppend name) t expr
-- renameDeclaration toAppend (DeclModule name decls) = DeclModule (append toAppend name) decls

instance Show Declaration' where
  show (DeclExpr expr) = "Expression: " ++ show expr
  show (DeclFun name type' expr) = "Function: " ++ unpack (removeMetadata name) ++ " : " ++ show type' ++ " = " ++ show expr
  show (DeclVal name literal) = "Value: " ++ unpack (removeMetadata name) ++ " = " ++ show literal
  show (DeclType name type') = "Type: " ++ unpack (removeMetadata name) ++ " = " ++ show type'
  show (DeclModule name decls) = "Module : " ++ unpack (removeMetadata name) ++ "[ " ++ concatMap show decls ++ " ]"
  show (DeclLoad filepath) = "Loading file: " ++ removeMetadata filepath
  
type Label = FullNode Text
type Field = (Label, Expression)

type Pattern = FullNode Pattern'

data Pattern' =
    PSumType Label [Pattern]
  | PVariable Label
  | PWildcard
  | PLiteral Literal
  | PDisjunctive Pattern Pattern
  | PAs Pattern Label
  deriving (Eq, Show)

type Expression = FullNode Expression'

data Expression'
    = ELiteral Literal
    | EVariable (FullNode Text)
    | EOperation Operator [Expression]
    | ELet LetSort [(FullNode Text, Expression)] Expression -- What about kind checking this?
    | EAbstraction (FullNode Text) Type (Maybe Type) Expression
    | EApplication Expression Expression
    | ECondition Expression Expression Expression
    | ETypeAbstraction TVariableInfo Kind (Maybe Type) Expression
    | ETypeApplication Expression Type
    | EList [Expression]
    | EAnonymousRecord [Field] -- TODO: We should remove lists from records, and just use a Map instead
    | ENominalRecord Type [Field]
    | EProgn [Expression]
    | ERecordProjection Expression Label
    | ERecordUpdate Expression [(Label, Expression)]
    | EAlgebraic Label [Expression]
    | EPatternMatching Expression [(Pattern, Maybe Expression, Expression)] 
    deriving (Eq, Show)

type Metadata = SourcePos

data FullNode a = FullNode Metadata a deriving Show

makeEmptyNode :: a -> FullNode a
makeEmptyNode = FullNode emptyMetadata
  where emptyMetadata = initialPos ""

makeNamedMetadata :: String -> Metadata
makeNamedMetadata = initialPos

instance Functor FullNode where
  fmap f (FullNode m v) = FullNode m (f v)

instance (Eq a) => Eq (FullNode a) where
  (FullNode _ a) == (FullNode _ b) = a == b

instance (Ord a) => Ord (FullNode a) where
  compare (FullNode _ a) (FullNode _ b) = compare a b

removeMetadata :: FullNode a -> a
removeMetadata (FullNode _ a) = a

getMetadata :: FullNode a -> Metadata
getMetadata (FullNode m _) = m
