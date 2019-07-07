module ZM.Parser.Value(value) where

import ZM.Parser.Lexer
import ZM.Parser.Util
import ZM.Parser.Types
import Text.Megaparsec

-- |Generic ZM value, a constructor followed by optional, optionally named, fields. 
data Value =
  Value String
        ValueFields
  deriving (Show)

-- |Constructor fields 
type ValueFields = Either [Value] [(String, Value)]

-- |Parse a document as a ZM value
-- valueD :: Parser Value
-- valueD = doc value
{-|
Parse a plain ZM value.
  
>>> parseMaybe value ""
Nothing
  
>>> parseMaybe value "False"
Just (Value "False" (Left []))
  
>>> parseMaybe value "(Cons False (Cons True Nil))"
Just (Value "Cons" (Left [Value "False" (Left []),Value "Cons" (Left [Value "True" (Left []),Value "Nil" (Left [])])]))

>>> parseMaybe value "Flag {val=True}"
Just (Value "Flag" (Right [("val",Value "True" (Left []))]))

Current limitations:

No special syntax for numbers,chars,strings:
>>> parseMaybe value "33"
Nothing

No type annotations:
>>> parseMaybe value "False::Bool"
Nothing
-}
value :: Parser Value
value = pars value <|> (Value <$> localId <*> fields)

simpleValue :: Parser Value
simpleValue = pars value <|> Value <$> localId <*> pure (Left [])

fields :: Parser ValueFields
fields = namedFields <|> unnamedFields

-- |Parse unnamed fields 
unnamedFields :: Parser ValueFields
unnamedFields = Left <$> many simpleValue

{-| Parse a set of named fields
  
>>> parseMaybe namedFields "{}"
Just (Right [])
  
Fields can be separated by commas:

>>> parseMaybe namedFields "{a=False,b=True}"
Just (Right [("a",Value "False" (Left [])),("b",Value "True" (Left []))])
  
Or just by spaces, but you might need to use parenthesis to avoid ambiguity.
  
This might be interpreted as 'False b' and so it fails: 

>>> parseMaybe namedFields "{a=False b=True}"
Nothing
  
>>> parseMaybe namedFields "{a=(False) b=True}"
Just (Right [("a",Value "False" (Left [])),("b",Value "True" (Left []))])
  
>>> parseMaybe namedFields "{a= Msg {from=Joe} b=True}"
Just (Right [("a",Value "Msg" (Right [("from",Value "Joe" (Left []))])),("b",Value "True" (Left []))])
  
Haskell style comments are allowed: 

>>> parseMaybe namedFields "{ l1  = cons false {- we are in the middle of a value-} nil , l2=  nil } -- named fields are completed"
Just (Right [("l1",Value "cons" (Left [Value "false" (Left []),Value "nil" (Left [])])),("l2",Value "nil" (Left []))])
  
No special syntax:
>>> parseMaybe namedFields "{a=3 b='a'}"
Nothing
-}
namedFields :: Parser ValueFields
namedFields = Right <$> cpars (sepBy namedField (optional $ symbol ","))

namedField :: Parser (String, Value)
namedField = do
  name <- localId
  _ <- symbol "="
  v <- value
  return (name, v)