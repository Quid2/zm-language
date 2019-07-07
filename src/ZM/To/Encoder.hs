{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- avoid warnings for test functions

{- Convert textual ZM values to their canonical binary representation -}
module ZM.To.Encoder
  ( encoderFor
  , typeEncoder
  -- , parseErrorPretty
  ) where

import           Control.Applicative.Permutations
import           Data.Bifunctor
import           Data.ByteString                  (ByteString)
import           Data.Flat.Encoder
import qualified Data.Map                         as M
import           Data.Monoid

-- import           Data.Scientific
import           Data.Void

-- import           Data.Word
import           Text.Megaparsec

--import           Text.Megaparsec.Char
-- import qualified Text.Megaparsec.Char.Lexer    as L
import           ZM                               hiding (Value)

import           ZM.Parser.ADT

-- import           ZM.Pretty
import           ZM.Parser.Lexer                  (charLiteral, float,
                                                   stringLiteral, symbol)
import           ZM.Parser.Types                  (Parser, TypeName, asTypeName)
import           ZM.Parser.Util                   (cpars, doc, pars)

-- |A direct converter from textual to binary format for ZM values
type ParserEncoder = String -> Either (ParseError Char Void) ByteString

-- |A mapping from an absolute type and its parser
type MapTypeEncoder = M.Map (Type AbsRef) (Parser OBJ)

-- |The result of the parsing of a ZM value: the number of encoded bits and the encoding.
type OBJ = (Sum NumBits, Encoding)

{-|
Return a Flat encoder for values of the indicated type.

$setup
>>> :set -XDeriveGeneric -XDeriveAnyClass -XRankNTypes -XScopedTypeVariables -XNoMonomorphismRestriction
>>> import Data.Bifunctor
>>> import Data.Flat.Bits
>>> import Test.QuickCheck

>>> newtype Msg1 = Msg1 { b0 :: Bool } deriving (Generic, Model, Flat, Show)
>>> data Msg3 = Msg3 { b1 :: Bool, b2 :: Bool, b3 :: Bool} deriving (Generic, Model, Flat, Show)
>>> let m1 = Msg1 True
>>> let m3 = Msg3 True False False
>>> let v1 = V 'j' "78金門" 4.4E34 (-9.9E-19)
>>> let enc = \p s -> bimap show prettyShow (encoderFor p s)
>>> enc (Proxy :: Proxy (Maybe Bool)) "Just True"
Right "[193]"

>>> enc (Proxy :: Proxy (Maybe Bool)) "(Just (True))"
Right "[193]"

>>> enc (Proxy :: Proxy Bool) "True:Bool.K306f1981b41c "
Right "[129]"

>>> enc (Proxy :: Proxy Bool) " True : Bool.K306f1981b41c "
Right "[129]"

Some types use a special syntax (Float, Double, Char, String, Integer):

>>> enc (Proxy :: Proxy Float) "3.1416:Int.Kb53bec846608"
Right "[64, 73, 15, 249, 1]"

-- BAD: we do not check the type name

>>> enc (Proxy :: Proxy Float) "3.1416:Float.Kb53bec846608"
Right "[64, 73, 15, 249, 1]"

-- >>> enc (Proxy :: Proxy Double) "3.1416:Int.Kb53bec846608"
-- Right "[64, 73, 15, 249, 1]"

>>> enc (Proxy :: Proxy Char) "'v'"
Right "[118, 1]"

>>> enc (Proxy :: Proxy (Maybe Bool)) "Just True:Int"
Right "[193]"

>>> enc (Proxy :: Proxy (Maybe Bool)) "Just True:Maybe (Bool.K306f1981b41c)"
Left "FancyError (SourcePos {sourceName = \"\", sourceLine = Pos 1, sourceColumn = Pos 37} :| []) (fromList [ErrorFail \"Type Mismatch: declared type is Maybe Bool.K306f1981b41c expected type is K306f1981b41c\"])"

-- >>> enc (Proxy :: Proxy (Maybe Bool)) "Just (True:Bool.K306f1981b41c)"
Right "[193]"

-- >>> enc (Proxy :: Proxy (Either (Maybe Bool) Int)) "Left (Just True): Either.K306f1981b41c (Maybe Bool.K306f1981b41c) Word8.K306f1981b41c"
-- >>> enc (Proxy :: Proxy (Either (Maybe Bool) Int)) "Left (Just True): Either (Maybe Bool.K306f1981b41c) Int"
Right "a"

$undisplayed
>>> ok False
True

>>> ok m1
True

>>> ok2 m1 "Msg1 (True)"
True

>>> ok2 m1 "(Msg1 {  b0 =(True) })"
True

>>> ok m3
True

prop> \(x::Char) -> ok x

>>> ok '金'
True

-- prop> \(x::String) -> ok x

>>> ok "!金金?"
True

-- prop> \(x::Float) -> ok x

-- prop> \(x::Double) -> ok x

-- prop> \(x::Integer) -> ok x

-- prop> \(v::Int) -> ok2 v (show v)

-- >>> ok2 m3 "Msg3 (True) False  False"
-- >>> ok2 m3 "Msg3 {b3=False,   b1=True,b2=False}"
-- >>> ok2 m3 "Msg3 {b3=False b1=True b2=False}"
-- >>> ok '金'
-- >>> ok "abc金"
-- >>> ok2 "ab"             "\"ab\""
-- >>> ok2 (-3.4 :: Float)  "-3.4"
-- >>> ok2 (-11 :: Float)   "-11"
-- >>> ok2 (11 :: Float)    "11"
-- >>> ok2 (11 :: Float)    "+11"
-- >>> ok2 (-11.1 :: Float) "-11.1"
-- >>> ok (4.4 :: Float)
-- >>> ok (-4.4 :: Double)
-- >>> ok2 v1 "V 'j' (\"78金門\") 4.4E34 ((-9.9E-19))"
-- >>> ok v1
-}
-- Check that the binary representation of a value is equal to the one produced by the encoding parser.
ok ::
     forall a. (Flat a, Model a, Show a)
  => a
  -> Bool
ok v = ok2 v (show v) -- (prettyShow v)

ok2 ::
     forall a. (Flat a, Model a)
  => a
  -> String
  -> Bool
ok2 v s =
  either (const False) (== flat v) $
  encoderFor (Proxy :: Proxy a) (" " ++ s ++ " ")

-- |Return a Flat encoder for values of the ZM type corresponding to the provided Haskell type
encoderFor :: Model a => Proxy a -> ParserEncoder
encoderFor = typeEncoder . absTypeModel

-- |Return a Flat encoder for values of the provided ZM type
typeEncoder :: AbsTypeModel -> ParserEncoder
typeEncoder tm =
  encodeWith $ doc (typeEncoder_ (typeEncoderMap tm) (typeName tm))

encodeWith :: Parser OBJ -> ParserEncoder
encodeWith pe s = flat <$> parse pe "" s

-- 'Fake' instance used to simplify the creation of the final binary representation
instance Flat OBJ where
  decode = undefined
  encode (_, enc) = enc
  size (Sum s, _) n = n + s

-- u = M.keys $ typeEncoderMap $ absTypeModel (Proxy :: Proxy V)
-- e = encodeWith encBool "(True ) "
-- l = shEnc (V 'k' "bb" 4.4 9.9E11)
-- z = encoderFor (Proxy :: Proxy Bool) "True"
-- m = prettyShow $ absTypeModel (Proxy :: Proxy Float)
-- data Msg1 = Msg1 {b0::Bool} deriving (Generic,Model,Flat,Show)
--instance Pretty Msg1 where pPrint = text . shob
--instance Pretty Msg3 where pPrint = text . show
--instance Pretty V where pPrint = text . show
data V = V
  { v0 :: Char
  , v1 :: String
  , v4 :: Float
  , v5 :: Double
  } deriving (Generic, Model, Flat, Show)

-- ff = flat (4.4 :: Double)
-- n = shEnc $ Msg3 True False True
-- shEnc
--   :: forall a
--    . (Show a, Model a)
--   => a
--   -> Either (ParseError Char Void) ByteString
-- shEnc v = encoderFor (Proxy :: Proxy a) (" " ++ show v ++ " ")
typeEncoderMap :: AbsTypeModel -> MapTypeEncoder
typeEncoderMap tm =
  let denv =
        addCustom (Proxy :: Proxy Double) doubleEnc .
        addCustom (Proxy :: Proxy Float) floatEnc .
        addCustom (Proxy :: Proxy Char) charEnc .
        addCustom (Proxy :: Proxy String) stringEnc $
        M.mapWithKey (\t ct -> conEnc denv t (Sum 0) mempty ct) (typeTree tm)
   in denv

-- |Add a custom parser for a type
addCustom ::
     Model a => Proxy a -> Parser OBJ -> MapTypeEncoder -> MapTypeEncoder
addCustom proxy parser env =
  let tm = absTypeModel proxy
   in M.insert (typeName tm) parser env

conEnc ::
     Convertible a String
  => MapTypeEncoder
  -> t
  -> Sum NumBits
  -> Encoding
  -> ConTree a AbsRef
  -> Parsec Void String OBJ
conEnc env t numBits enc ct =
  pars (conEnc env t numBits enc ct) <|> conEncoder env t numBits enc ct

conEncoder ::
     (Convertible a String)
  => MapTypeEncoder
  -> t
  -> Sum NumBits
  -> Encoding
  -> ConTree a AbsRef
  -> Parser OBJ
conEncoder env t numBits enc (ConTree l r) =
  let numBits' = (+ 1) <$> numBits
   in conEncoder env t numBits' (enc <> eFalse) l <|>
      conEncoder env t numBits' (enc <> eTrue) r
conEncoder env _ numBits enc (Con cn (Left ps)) =
  constrEnc (convert cn) numBits enc <> flds env ps
conEncoder env _ numBits enc (Con cn (Right ps)) =
  constrEnc (convert cn) numBits enc <> mnamedFlds env ps
  where
    mnamedFlds env ps =
      cpars (namedFlds (map (bimap convert (typeEncoder_ env)) ps)) <|>
      flds env (map snd ps)

flds :: MapTypeEncoder -> [AbsType] -> Parser OBJ
flds env ps = mconcat (map (typeEncoder_ env) ps)

constrEnc :: String -> Sum NumBits -> Encoding -> Parser OBJ
constrEnc name numBits enc = (numBits, enc) <$ symbol name

-- |Return an encoder for a given type
-- typeEncoder :: AbsTypeModel -> Parser OBJ
-- typeEncoder env tm = solve (t (typeEncoderMap tm)
typeEncoder_ :: MapTypeEncoder -> AbsType -> Parser OBJ
typeEncoder_ env typ = do
  let typeParser :: Parser (OBJ, Type (TypeName Identifier)) =
        (undefined, ) <$> (symbol ":" *> parType absTypeId)
      valueParser = (, undefined) <$> solve typ env
  ((r, _), mtp) <- (,) <$> valueParser <*> optional typeParser
  case mtp of
    Nothing -> return r
    -- BUG: no name check
    Just (_, dtyp) ->
      if dtyp == (asTypeName Nothing . Just <$> typ)
        then return r
        else fail $
             unwords
               [ "Type Mismatch: declared type is"
               , prettyShow dtyp
               , "expected type is"
               , prettyShow typ
               ]

{-
For a type such as :

data Msg = Msg {sent::Bool,content::String}

Parser for records fields, i.e.: sent=Bool,content="some message"
-}
namedFlds :: [(String, Parser OBJ)] -> Parser OBJ
namedFlds =
  (mconcat <$>) .
  intercalateEffect (optional (symbol ",")) .
  traverse (toPermutation . namedFld)

namedFld :: (String, Parser OBJ) -> Parser OBJ
namedFld (name, parser) = do
  _ <- symbol name
  _ <- symbol "="
  parser

floatEnc :: Parser OBJ
floatEnc = encParser (float :: Parser Float)

doubleEnc :: Parser OBJ
doubleEnc = encParser (float :: Parser Double)

charEnc :: Parser OBJ
charEnc = encParser charLiteral

stringEnc :: Parser OBJ
stringEnc = encParser stringLiteral

-- |Transform a parser in an encoding parser
encParser :: Flat a => Parser a -> Parser OBJ
encParser p = (\s -> (Sum $ getSize s, encode s)) <$> par p
  where
    par p = pars (par p) <|> p
-- dtBool = simpleConstr "False" <|> simpleConstr "True"
-- simpleConstr :: String -> Parser Value
-- simpleConstr name =
--   pars (simpleConstr name) <|> Value <$> symbol name <*> pure (Left [])
