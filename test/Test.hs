{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Test where

import           Control.Exception
import           Control.Monad
import           Data.List
import qualified Data.Map          as M
import           ZM
import           ZM.Parser
import           ZM.Pretty
import           ZM.Types

data List a = Nil | Cons { head :: a, tail :: List a } deriving (Generic,Flat,Model)

data Fix f a = Fix (f a) deriving (Generic,Flat)

data Fix2 f a = Fix2 (f a) deriving (Generic,Flat)

t = mapM_ (\(t,err) -> when (readDataTypes t /= Left (lines err)) (putStrLn $ unwords ["Incorrect result for",t,"got",show (either unlines show $ readDataTypes t)])) errs

n = readDataTypes $ showADTs (Proxy :: Proxy (List Bool))

y = putStrLn $ showADTs (Proxy :: Proxy (List Bool))
-- yy = readDataTypes

z = putStrLn sh
g  = readDataTypes sh

sh = prettyShow . typeEnv . absTypeModel $ (Proxy :: Proxy (List Bool))

showADTs proxy = let env = typeEnv . absTypeModel $ proxy
                 in intercalate ";\n" . map (prettyShow . (env,)) . M.elems $ env
                 -- in prettyShow $ env

tn = fmap (++"ADD") $ TypeN "Top" [TypeN "Bot1" [],TypeN "Bot2" [TypeN "Bot2.1" []]]

x = readDataTypes ";Bool = Fa>lse |True;boh = boh"
-- "List A ≡ Nil | Cons { head :: A, tail :: List A }\nBool = False |True"

-- t = map (\(d,exp) -> if parse d == exp then Nothing else Just (parse d)) results

-- parse :: String -> Either String [ADT String String (TypeRef String)]
-- parse = fmap M.fromList . parser . alexScanTokens

oks = ["Bool = False |True"
      ,"boh = boh"
      ,"a.D A ≡ D a.D"
      ,"a.List A ≡ Nil | Cons { head :: A, tail :: b.List A }"
      ,"是不是 = 是 | 不是"
      ]

errs = [
  ("List A A List B B C ≡ Nil | Cons { head :: A, tail :: List A }","In List: data type name equal to a variable\nIn List: Duplicated variables: A B")
  ,("DupConstructors = a | a | b | c | b","In DupConstructors: Duplicated constructors: a b")
  ,("a a;a.really.bad.adt a a b b c = C1 | D | C1 ","In a: data type name equal to a variable")
  ,("a.really.bad.adt a a b b = C1 | C1","In a.really.bad.adt: Too many components in qualified name 'a.really.bad.adt'\nIn a.really.bad.adt: Duplicated variables: a b\nIn a.really.bad.adt: Duplicated constructors: C1")
  ,("Either a b = Left a | Right b;T a = T (Either a)","In T: Incorrect application of Either, should have 2 parameters but has 1\n")
  ,("Bool = False;Maybe a = Just a | Nothing;Either a b = Left a | Right b;G g = g g;T a = T (Either (Maybe a) (Either (Maybe a) Bool Maybe))","In T: Incorrect application of Either, should have 2 parameters but has 3\nIn T: Incorrect application of Maybe, should have 1 parameters but has 0")
  ,("List a b = C (List a)","In List: Incorrect application of List, should have 2 parameters but has 1\n")
  ,("Fix f a = Fix (f a)","In Fix: Incorrect application of parameter number 0, should have 0 parameters but has 1\n")
  ,("FixR f a = FixR (f (f a))","In FixR: Incorrect application of parameter number 0, should have 0 parameters but has 1\nIn FixR: Incorrect application of parameter number 0, should have 0 parameters but has 1\n")
  ,("Free f a = Pure a | Free (f (Free f a))","In Free: Incorrect application of parameter number 0, should have 0 parameters but has 1\n")
  ,("A = A B;B = B A;C = C D;D = D C A","Found mutually recursive types [D, C]\nFound mutually recursive types [B, A]\n")
  ,("A = A B;B = B C;C = C A","Found mutually recursive types [C, B, A]")
  ,("a.nother.long.tree = Left a.tree | Right b.tree","In a.nother.long.tree: Too many components in qualified name 'a.nother.long.tree'")
  ,("a.D A ≡ D b.D","transitiveClosure:Unknown reference to b.D\n")
  ,("a.tree = Left a.tree | Right b.tree","transitiveClosure:Unknown reference to b.tree\n")
  ,("a = | |","syntax error  |  | \n")
  ,(" ! ","lexical error at line 1, column 2\n")
  ,("= = =","syntax error  =  =  = \n")
  ,("a b.d e.f = c.f","In a: In c.f: '.' is not an Unicode Letter or Number or a _\nIn a: In b.d: '.' is not an Unicode Letter or Number or a _\nIn a: In e.f: '.' is not an Unicode Letter or Number or a _\n")
  ]

shouldBeOk = [

             ]
