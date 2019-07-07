{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module ZM.To.Util where

import           Control.Monad
import qualified Data.Map                      as M
-- import qualified Data.Set                      as Set
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           System.Directory
import           System.FilePath
import           ZM                      hiding ( dotted )
import           Data.Maybe

data WriteFlags = WriteFlags
  { srcDir :: String  -- ^ Directory to write source code to
  , overwrite :: Bool -- ^ True if existing modules should be overwritten 
  }

data Flags primType = Flags
  { namespace :: [String] -- ^ ["ZM","ADT"]
  , primTypes :: M.Map T.Text primType -- ^ should be Set.Set primType?
  , addIndex :: Bool -- ^ Add an index module
  }

namespaceT :: Flags p -> [T.Text]
namespaceT = map T.pack . namespace

data Module = Module
  { mdlExtension :: String
  , mdlPath :: [T.Text]
  , mdlContent :: T.Text
  } deriving (Show)

hsModule :: [T.Text] -> T.Text -> Module
hsModule = Module "hs"
tsModule :: [T.Text] -> T.Text -> Module
tsModule = Module "ts"

mdlName :: Module -> T.Text
mdlName = dotted . mdlPath

mdlFilePath :: Module -> FilePath
mdlFilePath mdl = joinPath (map convert $ mdlPath mdl) <.> mdlExtension mdl

mdlWrite :: WriteFlags -> Module -> IO ()
mdlWrite flags mdl = do
  let f = srcDir flags </> mdlFilePath mdl
  createDirectoryIfMissing True (takeDirectory f)
  exists <- doesFileExist f
  --print f >> print exists
  when (overwrite flags || not exists) $ T.writeFile f (mdlContent mdl)
  putStrLn f
  T.putStrLn (mdlContent mdl)

--instance Default Flags where def = Flags {srcDir=".",namespace=["ZM","ADT"],primTypes=["Word8.Kb1f46a49c8f8","Word7.Kf4c946334a7e"]}
-- isPrimType :: Convertible a String => Flags p -> a -> Bool
--isPrimType flags typ = Set.member (convert typ) (Set.fromList $ primTypes flags)
--isPrimType flags typ = Set.member (asT typ) (Set.map asT $ primTypes flags)

isPrimType :: Convertible a1 String => Flags a2 -> a1 -> Bool
isPrimType flags typ = isJust $ primType flags typ

primType :: Convertible a1 String => Flags a2 -> a1 -> Maybe a2
primType flags typ = M.lookup (asT typ) (primTypes flags)

-- |True if zero or more constructors with no parameters
isEnum :: ADT name a ref -> Bool
isEnum adt = case declCons adt of
  Just ct -> all noPars . constructors $ ct
  Nothing -> True
  where noPars (_, fs) = null . fieldsTypes $ fs

-- |True if single constructor with just one parameter
isNewType :: ADT name consName ref -> Bool
isNewType adt = case declCons adt of
  Just (Con _ (Left  [_])) -> True
  Just (Con _ (Right [_])) -> True
  _                        -> False

moduleName_
  :: (Convertible String a, Pretty a2) => ([a] -> t) -> [a] -> a -> a2 -> t
moduleName_ f namespace adtName adtRef =
  f $ namespace ++ moduleShortName_ adtName adtRef

moduleShortName :: Pretty a2 => T.Text -> a2 -> T.Text
moduleShortName adtName adtRef = dotted $ moduleShortName_ adtName adtRef

moduleShortName_ :: (Pretty a2, Convertible String a1) => a1 -> a2 -> [a1]
moduleShortName_ adtName adtRef = [adtName, convert $ prettyShow adtRef]

declNameS
  :: (Ord k, Show k, Convertible a String)
  => M.Map k (ADT a consName ref)
  -> k
  -> T.Text
declNameS adtEnv adtRef = declNameT $ solve adtRef adtEnv

declNameT :: Convertible a String => ADT a consName ref -> T.Text
declNameT = asT . declName

asT :: Convertible a String => a -> T.Text
asT = T.pack . convert

showT :: Show a => a -> T.Text
showT = asT . show

dotted :: [T.Text] -> T.Text
dotted = T.intercalate "."

list :: p -> ([a] -> p) -> [a] -> p
list nil _ [] = nil
list _   f l  = f l
