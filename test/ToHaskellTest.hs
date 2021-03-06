{-# LANGUAGE TemplateHaskell #-}

module ToHaskellTest
  ( tests
  , updateReferenceFiles
  )
where

import           Control.Arrow
import           Data.Bifunctor
import           Data.FileEmbed
import           Data.Int
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Word
import           Info                           ( models )
import           System.FilePath
import           Test.Tasty
import           Test.Tasty.HUnit
import           ZM
import           ZM.Parser
import           ZM.To.Haskell
import           ZM.Type.Bits11
import           ZM.Type.Bits23
import           ZM.Type.Bits52
import           ZM.Type.Bits8
import           ZM.Type.Float32
import           ZM.Type.Float64
import           ZM.Type.List
import           ZM.Type.Map
import           ZM.Type.NonEmptyList
import           ZM.Type.Tuples
import           ZM.Type.Unit
import           ZM.Types
import qualified Data.ByteString               as B

-- import           Test.ZM.ADT.All -- Compile all generated files

-- write new reference files (not embedded though till this file is recompiled)
updateReferenceFiles = mapM_
  (mdlWrite WriteFlags { srcDir    = "/Users/titto/workspace/zm-language/test"
                       , overwrite = True
                       }
  )
  genFiles

t = defaultMain tests

tests = testGroup "ToHaskell" $ map tst latestFiles
 where
  tst (k, latestFiles) =
    let Just reference = M.lookup k refs
    in  testCase (unwords ["To Haskell", k]) (latestFiles @?= reference)
  refs = M.fromList referenceFiles


latestFiles, referenceFiles :: [(FilePath, T.Text)]
latestFiles = map (mdlFilePath &&& mdlContent) genFiles

referenceFiles = map ((genDir </>) *** convert) refFiles
  where genDir = "Test/ZM"

-- [("ADT/All.hs","module Test.ZM.ADT.All..."),..]
refFiles :: [(FilePath, B.ByteString)]
refFiles = $(embedDir "test/Test/ZM")


-- [Module {mdlPath = ["Test","ZM","ADT","All"]..]
genFiles = generate
  Flags { namespace = ["Test", "ZM", "ADT"]
        , primTypes = defaultPrimitiveTypes
        , addIndex  = False
        }
  (absEnv (Proxy :: Proxy AbsADT))

-- g = mapM_ (mdlWrite WriteFlags {srcDir="/Users/titto/workspace/zm-language/test",overwrite=False}) $ generate Flags {namespace=["Test","ZM","ADT"],primTypes=[],addIndex=True} (absEnv (Proxy :: Proxy AbsADT))
-- t = sh (Proxy :: Proxy Bits11)
-- sh p = let env = typeEnv . absTypeModel $ p
--            ref = last . M.keys $ env
--        in T.putStr $ gen fs env ref
{-
fs = Flags {srcDir = "/Users/titto/workspace/zm/test",namespace = ["ZM","ADT"]}

envs = let tm = typeEnv . absTypeModel
       in M.unions [tm (Proxy :: Proxy AbsADT)
                       ,tm (Proxy :: Proxy IEEE_754_binary32)
                       ,tm (Proxy :: Proxy IEEE_754_binary64)
                       ,tm (Proxy :: Proxy Word)
                       ,tm (Proxy :: Proxy Word64)
                       ,tm (Proxy :: Proxy Word32)
                       ,tm (Proxy :: Proxy Word16)
                       ,tm (Proxy :: Proxy Word8)
                       ,tm (Proxy :: Proxy Int)
                       ,tm (Proxy :: Proxy Int64)
                       ,tm (Proxy :: Proxy Int32)
                       ,tm (Proxy :: Proxy Int16)
                       ,tm (Proxy :: Proxy Int8)
                       ]

                                 -- tm (Proxy :: Proxy Bits11)
          -- <> tm (Proxy :: Proxy Bits23)
          -- <> tm (Proxy :: Proxy Bits52)
          -- <> tm (Proxy :: Proxy Bits8)
          -- <> tm (Proxy :: Proxy IEEE_754_binary32)
          -- <> tm (Proxy :: Proxy IEEE_754_binary64)
          -- <> tm (Proxy :: Proxy (List Bits11))
          -- <> tm (Proxy :: Proxy (Map Bits11 Bits11))
          -- <> tm (Proxy :: Proxy (NonEmptyList Bits11))
          -- <> tm (Proxy :: Proxy (Tuple2 Bits11 Bits11)) -- Tuple3 ..
          -- <> tm (Proxy :: Proxy Unit)
          -- <> tm (Proxy :: Proxy Word)
          -- <> tm (Proxy :: Proxy AbsADT)
-}
