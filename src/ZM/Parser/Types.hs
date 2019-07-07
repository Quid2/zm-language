{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiParamTypeClasses,
  FlexibleInstances, DeriveFunctor #-}

module ZM.Parser.Types
  ( Parser
  , ADTParts(..)
  , asTypeName
  , hasName
  , hasRef
  , typeNameName
  , typeNameRef
  , TypeName
  , localName
  -- , asQualName
  , Label(..)
  , At
  , AtId
  , AtAbsName
  , AtError
  , Range(..)
  , module Data.Either.Validation
  )
where

import           Data.Either.Validation
import           Data.Void
import           Data.Word
import           Text.Megaparsec         hiding ( Label
                                                , label
                                                )
import qualified Text.PrettyPrint              as P
import           Text.PrettyPrint        hiding ( (<>) )
import           ZM                      hiding ( Value )
--import           Data.Maybe
import Data.These

type Parser = Parsec Void String -- TODO: generalise to any textual type

-- |A parsed ADT 
data ADTParts = ADTParts
  { name :: AtAbsName
  , vars :: [AtId]
  , constrs :: [(AtId, Fields AtId AtAbsName)]
  } deriving (Show)

instance Pretty ADTParts where
  pPrint p =
    pPrint (name p) <+> hsep (map pPrint $ vars p) <+> char '=' <+> hsep
      (punctuate (text " |") (map pPrint (constrs p)))
  --text "ADTParts" <+> pPrint (name p) <+> pPrint (vars p) <+> pPrint (constrs p)

-- | A data type name can be either local, or absolute, or both: "Bool" | "Bool.K306f1981b41c" | ".K306f1981b41c"
type TypeName l = These l AbsRef
-- TODO: Convert to These l AbsRef
-- data TypeName l =
--   TypeName l
--           (Maybe AbsRef)
--   deriving (Show, Functor, Ord, Eq)
-- localName (TypeName l _) = l

-- asQualName (TypeName l Nothing) = QualName "" "" (prettyShow l)
-- asQualName (TypeName l (Just ref)) = QualName "" (prettyShow l) (prettyShow ref)

-- instance Pretty n => Pretty (TypeName n) where
--   pPrint (TypeName n Nothing) = pPrint n
--   pPrint (TypeName n (Just r)) = pPrint n <> char '.' <> pPrint r

asTypeName :: Maybe a -> Maybe b -> These a b
asTypeName = these

typeNameName :: TypeName l -> l
typeNameName = this

typeNameRef :: TypeName l -> AbsRef
typeNameRef = that

hasName :: These a b -> Bool
hasName = hasThis

hasRef :: These a1 a -> Bool
hasRef = hasThat

localName :: These c b -> c
localName = this

instance Pretty n => Pretty (TypeName n) where
  pPrint = both pPrint (\r -> char '.' P.<> pPrint r)

-- This a == These a (any b) 

data Range = Range
  { line, start, end :: Word32
  } deriving (Show, Eq, Ord, Generic, Flat, Model)

instance Pretty Range where
  pPrint r = text $ concat
    [ "("
    , show $ line r
    , ":"
    , show $ start r
    , if end r == start r then "" else "-" ++ show (end r)
    , ")"
    ]

type At v = Label Range v

type AtError = At String

type AtId = At Identifier

type AtAbsName = At (TypeName Identifier)

--- 'Transparent' position-marked envelope
data Label l a = Label
  { label :: l
  , object :: a
  } deriving (Show, Functor)

instance (Pretty l,Pretty a) => Pretty (Label l a) where
  pPrint (Label l a) = pPrint a <> text "@" <> pPrint l

instance Eq p => Eq (Label l p) where
  (Label _ a) == (Label _ b) = a == b

instance Ord a => Ord (Label l a) where
  (Label _ a) <= (Label _ b) = a <= b

instance Convertible a b => Convertible (Label l a) (Label l b) where
  safeConvert (Label l a) = Label l <$> safeConvert a

instance Convertible a b => Convertible (Label l a) b where
  safeConvert (Label _ a) = safeConvert a

-- MOVE TO model 
-- instance Convertible a a where safeConvert = Right
