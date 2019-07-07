{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module ToZMTTest
  ( tests
  ) where

import qualified Data.Text        as T
import           Test.Tasty
import           Test.Tasty.HUnit
import           ZM
import           ZM.To.ZMT

t = defaultMain tests

tests =
  testGroup
    "To ZMT"
  -- tst False (Proxy :: Proxy (Data2.List [Bool])) "Bool \8801 False | True; List.K9437b401bfa3 a \8801 Cons2 a (List.K9437b401bfa3 a) | Nil2; List.Kb8cd13187198 a \8801 Nil | Cons a (List.Kb8cd13187198 a)"
  -- ,tst True (Proxy :: Proxy (Data2.List [Bool])) "Bool.K306f1981b41c \8801 False | True; List.K9437b401bfa3 a \8801 Cons2 a (List.K9437b401bfa3 a) | Nil2; List.Kb8cd13187198 a \8801 Nil | Cons a (List.Kb8cd13187198 a)"
    []
  where
    tst fullyQualified p s =
      let tm = absTypeModel p
       in testCase (unwords ["To ZMT", prettyShow $ typeName tm]) $
          (norm . generate fullyQualified . typeEnv $ tm) @?= s

norm = T.intercalate " " . T.words
