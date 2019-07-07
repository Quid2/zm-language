{-# LANGUAGE NoMonomorphismRestriction, GADTs #-}

module ZM.Parser.Lexer
  ( sc
  , eof
  -- $lexemes
  , localId
  , symbol
  , float
  , charLiteral
  , stringLiteral
  , shake
  ) where

import Data.Scientific
-- import Data.Word
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import ZM hiding ()
import ZM.Parser.Types (Parser)

{- $lexemes
Lexemes remove any trailing space, including comments:

>>> parseMaybe float "3.3  -- a nice float"
Just 3.3

but do not remove initial space:

>>> parseMaybe float "  3.3"
Nothing

an example of a parsing error:

>>> parse float "" "  3.3"
Left (TrivialError (SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1} ...
-}

{-|
>>> parseMaybe charLiteral "''"
Nothing

>>> parseMaybe charLiteral " 'a'"
Nothing

>>> parseMaybe charLiteral "'a' -- a comment"
Just 'a'

>>> parseMaybe charLiteral "'a'"
Just 'a' 

>>> parseMaybe charLiteral "'\n'"
Just '\n'

>>> parseMaybe charLiteral "'金'"
Just '\37329'

>>> parseMaybe charLiteral "'\37329'"
Just '\37329'
-}
charLiteral :: Parser Char
charLiteral = lexeme $ between (char '\'') (char '\'') L.charLiteral

{-|
>>> parseMaybe stringLiteral "\"\""
Just ""

>>> parseMaybe stringLiteral "\"abc\n金\37329\" "
Just "abc\n\37329\37329"
-}
stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

{-|
Parse signed floats or integers (as floats)

>>> parseMaybe float "3"
Just 3.0

>>> parseMaybe (float :: Parser Float)  "+3" 
Just 3.0

>>> parseMaybe (float :: Parser Double) "-3"
Just (-3.0)

>>> parseMaybe float "3.6E+11"
Just 3.6e11

>>> parseMaybe float "-3.6E-11"
Just (-3.6e-11)

>>> parseMaybe float "-1E99999999"
Just (-Infinity)
-}
float :: RealFloat a => Parser a
float = toRealFloat <$> L.signed (return ()) (lexeme L.scientific)

{-|
Parse a ZM localId (a unicode letter followed by zero or more unicode alphanumeric characters or '_')

>>> parseMaybe localId "*"
Nothing

>>> parseMaybe localId "1"
Nothing

>>> parseMaybe localId "A"
Just "A"

>>> parseMaybe localId "Gold金en"
Just "Gold\37329en"

>>> parseMaybe localId "是不是"
Just "\26159\19981\26159"

>>> parseMaybe localId "Bool -- a bool"
Just "Bool"

>>> parseMaybe localId "ant_13_"
Just "ant_13_"
-}
localId :: Parser String
localId = lexeme p
  where
    p = (:) <$> letterChar <*> many (alphaNumChar <|> char '_')

{-|
Parse a specific string

>>> parseMaybe (symbol "=") ""
Nothing

>>> parseMaybe (symbol "=") "*"
Nothing

>>> parseMaybe (symbol "=") "= -- an equal sign"
Just "="

>>> parseMaybe (symbol "if then") "if then"
Just "if then"

>>> parseMaybe (symbol "Gold金en") "Gold金en"
Just "Gold\37329en"
-}
symbol :: String -> Parser String
symbol = L.symbol sc

{-|
Parse absolute reference's compact string format

>>> parseMaybe shake "Ke45682c11f7b"
Just (SHAKE128_48 228 86 130 193 31 123)

>>> parseMaybe shake "KE45682C11F7B "
Just (SHAKE128_48 228 86 130 193 31 123)
-}
shake :: Parser (SHAKE128_48 a)
shake = lexeme k
  where
    k =
      (\k0 k1 k2 k3 k4 k5 k6 k7 k8 k9 k10 k11 k12 ->
         unPrettyRef [k0, k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12]) <$>
      char 'K' <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar <*>
      hexDigitChar

-- |Space consumer
-- |Removes spaces and haskell style line and block comments "--.." "{- ..-}"  
sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment = L.skipLineComment "--"
    blockComment = L.skipBlockComment "{-" "-}"

--end parser = (<* eof)
--doc = between sc (sc >> eof)
-- |Add trailing space removal to a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
