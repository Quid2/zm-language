{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Main where

import           Test.Tasty

-- import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck as QC
-- import           Text.PrettyPrint
import qualified ParserTest
import qualified ToHaskellTest
import qualified ToZMTTest

t = main

main = do
  ToHaskellTest.updateReferenceFiles
  defaultMain tests

tests :: TestTree
tests =
  testGroup "Tests" [ParserTest.tests, ToZMTTest.tests, ToHaskellTest.tests]
