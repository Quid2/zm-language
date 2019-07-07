{-# LANGUAGE TemplateHaskell #-}
module ToHaskellTest (tests) where

import           Info             (models)
import           Test.Tasty
import           ZM.Parser
import           ZM.Types
import           ZM
import           Test.Tasty.HUnit
import           ZM.Type.Bits11
import           ZM.Type.Bits23
import           ZM.Type.Bits52
import           ZM.Type.Bits8
import           ZM.Type.Float32
import           ZM.Type.Float64
import           ZM.Type.List
import ZM.Type.Map
import ZM.Type.NonEmptyList
import ZM.Type.Tuples
import ZM.Type.Unit
import Data.Int
import Data.Word
import ZM.To.Haskell
import Data.FileEmbed
import qualified Data.Map as M
import Data.Bifunctor
import System.FilePath
import qualified Data.Text as T
import Control.Arrow


-- import Test.ZM.ADT.All -- Compile all generated files

updateFiles = mapM_ (mdlWrite WriteFlags {srcDir="/Users/titto/workspace/zm-language/test",overwrite=True}) genFiles

t = defaultMain tests

tests = testGroup "ToHaskell" $ map tst gFiles
  where
    tst (k,cnt) = let Just rcnt = M.lookup k refs in testCase (unwords ["To Haskell",k]) (cnt @?= rcnt)
    refs = M.fromList rFiles

gFiles,rFiles :: [(FilePath,T.Text)]
gFiles = map (mdlFilePath &&& mdlContent) genFiles
rFiles = map ((genDir </>) *** convert) refFiles

genDir = "Test/ZM"
-- [("ADT/All.hs","module Test.ZM.ADT.All..."),..]
refFiles = $(embedDir "test/Test/ZM")


-- [Module {mdlPath = ["Test","ZM","ADT","All"]..] 
genFiles = generate Flags {namespace=["Test","ZM","ADT"],primTypes=defaultPrimitiveTypes,addIndex=False} (absEnv (Proxy :: Proxy AbsADT))

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