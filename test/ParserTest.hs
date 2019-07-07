{-# LANGUAGE FlexibleContexts #-}

-- Unncessary? Are these tests already in the code as doctests?
module ParserTest
  ( tests
  ) where

import           Data.Bifunctor
import           Info             (models)
import           Test.Tasty
import           Test.Tasty.HUnit
import           ZM
import           ZM.Parser
import           ZM.Types

t = defaultMain tests

tests =
  testGroup "Parser Tests" [test_parser_oks, test_parser_errs, test_roundtrip]

test_parser_oks = testGroup "Parse" $ map tst oks
  where
    tst d =
      testCase (unwords ["Parse", d]) $ (prettyShow <$> parseADTs d) @?= Right d

test_parser_errs = testGroup "Parse Errs" $ map tst errs
  where
    tst (d, exp) =
      testCase (unwords ["Parse Err", d]) $
      first (map prettyShow) (parseADTs d) @?= Left (lines exp)

-- Parsed model is identical to original model
test_roundtrip = testGroup "Model" $ map tst models
  where
    tst model =
      testCase
        (unwords
           [ "Model"
                        -- ,prettyShow $ typeEnv model
           ]) $
      let env = typeEnv model
       in (parseADTs . prettyShow $ env) @?= Right env

--t = map (\(d,exp) -> if parser d == exp then Nothing else Just (parser d)) results
-- parse :: String -> Either String [ADT String String (TypeRef String)]
--parse = parser . alexScanTokens
results = []

oks = []

errs = []
