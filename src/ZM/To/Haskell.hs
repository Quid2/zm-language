{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module ZM.To.Haskell
  ( WriteFlags(..)
  , Flags(..)
  , defaultPrimitiveTypes
  , generate
  , Module(..)
  , mdlFilePath
  , mdlWrite
  )
where

--import Control.Monad
import qualified Data.Map                      as M
--import qualified Data.Set as Set
import qualified Data.Text                     as T
--import qualified Data.Text.IO as T
import           ZM                      hiding ( dotted
                                                , mdlName
                                                , moduleName
                                                ) -- ?
import           ZM.To.Util
--import Data.String

-- t = mapM_ (mdlWrite WriteFlags {srcDir="/Users/titto/workspace/zm-language/test",overwrite=False}) $ generate Flags {namespace=["Test","ZM","ADT"],primTypes=[],addIndex=True} (absEnv (Proxy :: Proxy [Bool]))

type HFlags = Flags T.Text

defaultPrimitiveTypes :: M.Map T.Text T.Text
defaultPrimitiveTypes = M.fromList $ map
  (\v -> (v, v))
  ["Word8.Kb1f46a49c8f8", "Word7.Kf4c946334a7e", "Array.K2e8b4519aeaa"]

{-|
Generate an Haskell module for every absolute ADT plus, if required, an index module.
-}
generate :: HFlags -> AbsEnv -> [Module]
generate flags env =
  let mdls = map (generateModule flags env) (M.toList env)
  in  if addIndex flags then generateIndex flags mdls : mdls else mdls

generateIndex :: HFlags -> [Module] -> Module
generateIndex flags mdls =
  let mdlNames   = map mdlName mdls
      allContent = T.unlines
        ( T.unwords ["module", dotted (namespaceT flags ++ ["All where"])]
        : map (\n -> T.unwords ["import", n, "()"]) mdlNames
        )
  in  hsModule (namespaceT flags ++ ["All"]) allContent

generateModule :: HFlags -> AbsEnv -> (AbsRef, AbsADT) -> Module
generateModule flags adtEnv (adtRef, adt) =
  let
    ns          = namespaceT flags
    -- e.g. Bit
    name        = declNameT adt
    -- e.g. Bit.K65149ce3b366
    absName     = moduleShortName name adtRef
    -- e.g. ZM.ADT.Bit.K65149ce3b366
    mdlName     = moduleName ns name adtRef
    -- e.g. ["ZM","ADT","Bit","K65149ce3b366"]
    mdlNameC    = moduleName_ id ns name adtRef
    contentNorm = T.unlines
      [ "{-# LANGUAGE DeriveAnyClass #-}"
      , "{-# LANGUAGE DeriveGeneric  #-}"
      , header
      , "import qualified Prelude(Eq,Ord,Show)"
      , "import qualified GHC.Generics"
      , "import qualified Data.Flat"
      , "import qualified Data.Model"
      , T.unlines
      $ map
          (\ref -> T.unwords
            ["import qualified", moduleName ns (declNameS adtEnv ref) ref]
          )
      $ innerReferences adt
      , convert
      $ prettyShow
      $ prettyADT (if isNewType adt then "newtype " else "data ") '='
      $ substAbsADT (\ref -> adtFullName ns (declNameS adtEnv ref) ref) adt
      , "  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Data.Flat.Flat)"
      , instanceADT "Data.Model.Model" adt
      ]
    contentPrim =
      T.unlines [headerPrim, T.concat ["import ZM.Prim(", declNameT adt, ")"]]
    headerPrim = T.unwords ["module", mdlName, "(module ZM.Prim) where"]
    header =
      T.unwords ["module", mdlName, T.concat ["(", name, "(..)", ")"], "where"]
    content = if isPrimType flags absName then contentPrim else contentNorm
  in
    hsModule mdlNameC content

--declNameT = convert . adtNameS

--adtNameS :: Convertible a String => ADT a consName ref -> String
--adtNameS adt = convert (declName adt) :: String

-- moduleRelFile = moduleName_ modulePath

-- modulePath ps = joinPath (map convert ps) <.> "hs"

-- ZM.Type.Bit.K65149ce3b366
moduleName :: Pretty a2 => [T.Text] -> T.Text -> a2 -> T.Text
moduleName = moduleName_ dotted

-- ZM.ADT.Bit.K65149ce3b366.Bit
adtFullName :: Pretty a => [T.Text] -> T.Text -> a -> T.Text
adtFullName namespace adtName adtRef =
  dotted [moduleName namespace adtName adtRef, adtName]

instanceADT
  :: Convertible name String => T.Text -> ADT name consName ref -> T.Text
instanceADT cls adt =
  instanceCtx cls (declNameT adt) (fromIntegral . declNumParameters $ adt)

instanceCtx :: T.Text -> T.Text -> Int -> T.Text
instanceCtx cls dt 0 = T.unwords ["instance", cls, dt]
instanceCtx cls dt n =
  let vs = map T.singleton $ take n ['a' ..]
  in  T.unwords
        [ "instance ("
        , T.intercalate "," . map (\v -> T.unwords [cls, v]) $ vs
        , ") =>"
        , cls
        , "("
        , dt
        , T.unwords vs
        , ")"
        ]
