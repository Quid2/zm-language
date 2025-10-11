{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ZM.To.Util where

-- import qualified Data.Set                      as Set

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Char
import Data.Foldable
import Data.Int
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word
import Network.Top.Repo
import Network.Top.Run
import Numeric.Natural
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath
import Text.PrettyPrint (Doc, render, vcat)
import ZM hiding (chr, dotted)

-- | Monad stack for code generation: Reader for AbsEnv, Writer for code fragments
type CodeGen = ReaderT AbsEnv (Writer [Doc])

emit :: Doc -> CodeGen ()
emit d = lift $ tell [d]

getEnv :: CodeGen AbsEnv
getEnv = ask

runCodeGen :: AbsEnv -> CodeGen a -> Doc
runCodeGen env cg = vcat $ execWriter (runReaderT cg env)

{-
>>> error $ t (absEnv (Proxy:: Proxy [Bool]))
Bool.K306f1981b41c ≡   False
                     | True;
List.Kb8cd13187198 a ≡   Nil
                       | Cons a (List.Kb8cd13187198 a);
-}
t env = render $ runCodeGen env cg
  where
    cg = do
      adt <- ask
      emit $ text $ prettyShow adt

data WriteFlags
  = WriteFlags
  { -- | Directory to write source code to
    srcDir :: String,
    -- | True if existing modules should be overwritten
    overwrite :: Bool
  }

data Flags primType
  = Flags
  { -- | ["ZM","ADT"]
    namespace :: [String],
    -- | should be Set.Set primType?
    primTypes :: M.Map T.Text primType,
    -- | Add an index module
    addIndex :: Bool
  }

namespaceT :: Flags p -> [T.Text]
namespaceT = map T.pack . namespace

data Module
  = Module {mdlExtension :: String, mdlPath :: [T.Text], mdlContent :: T.Text}
  deriving (Show)

hsModule :: [T.Text] -> T.Text -> Module
hsModule = Module "hs"

dartModule :: [T.Text] -> T.Text -> Module
dartModule = Module "dart"

tsModule :: [T.Text] -> T.Text -> Module
tsModule = Module "ts"

pursModule :: [T.Text] -> T.Text -> Module
pursModule = Module "purs"

pursForeignModule :: [T.Text] -> T.Text -> Module
pursForeignModule = Module "js"

mdlName :: Module -> T.Text
mdlName = dotted . mdlPath

mdlFilePath :: Module -> FilePath
mdlFilePath mdl = joinPath (map convert $ mdlPath mdl) <.> mdlExtension mdl

mdlWrite :: WriteFlags -> Module -> IO ()
mdlWrite flags mdl = do
  let f = srcDir flags </> mdlFilePath mdl
  createDirectoryIfMissing True (takeDirectory f)
  exists <- doesFileExist f
  -- print f >> print exists
  when (overwrite flags || not exists) $ T.writeFile f (mdlContent mdl)
  putStrLn f
  T.putStrLn (mdlContent mdl)

-- instance Default Flags where def = Flags {srcDir=".",namespace=["ZM","ADT"],primTypes=["Word8.Kb1f46a49c8f8","Word7.Kf4c946334a7e"]}
-- isPrimType :: Convertible a String => Flags p -> a -> Bool
-- isPrimType flags typ = Set.member (convert typ) (Set.fromList $ primTypes flags)
-- isPrimType flags typ = Set.member (asT typ) (Set.map asT $ primTypes flags)
isPrimType :: (Convertible a1 String) => Flags a2 -> a1 -> Bool
isPrimType flags typ = isJust $ primType flags typ

primType :: (Convertible a1 String) => Flags a2 -> a1 -> Maybe a2
primType flags typ = M.lookup (asT typ) (primTypes flags)

-- | True if zero or more constructors with no parameters
isEnum :: ADT name a ref -> Bool
isEnum adt = case declCons adt of
  Just ct -> all noPars . constructors $ ct
  Nothing -> True
  where
    noPars (_, fs) = null . fieldsTypes $ fs

-- | True if single constructor with just one parameter
isNewType :: ADT name consName ref -> Bool
isNewType adt = case declCons adt of
  Just (Con _ (Left [_])) -> True
  Just (Con _ (Right [_])) -> True
  _ -> False

moduleName_ ::
  (Convertible String a, Pretty a2) => ([a] -> t) -> [a] -> a -> a2 -> t
moduleName_ f namespace adtName adtRef =
  f $ namespace ++ moduleShortName_ adtName adtRef

moduleShortName :: (Pretty a2) => T.Text -> a2 -> T.Text
moduleShortName adtName adtRef = dotted $ moduleShortName_ adtName adtRef

moduleShortName_ :: (Pretty a2, Convertible String a1) => a1 -> a2 -> [a1]
moduleShortName_ adtName adtRef = [adtName, convert $ prettyShow adtRef]

declNameS ::
  (Ord k, Show k, Convertible a String) =>
  M.Map k (ADT a consName ref) ->
  k ->
  T.Text
declNameS adtEnv adtRef = declNameT $ solve adtRef adtEnv

declNameT :: (Convertible a String) => ADT a consName ref -> T.Text
declNameT = asT . declName

-- >>> var 1
-- "B"
var :: (Integral a) => a -> T.Text
var = var_ 'A'

var_ :: (Integral a) => Char -> a -> T.Text
var_ start = T.singleton . chr . (ord start +) . fromIntegral

prettyT :: (Pretty a) => a -> T.Text
prettyT = T.pack . prettyShow

namedFields :: (Convertible a String) => Either [b] [(a, b)] -> [(T.Text, b)]
namedFields = namedFieldsWith (\n -> "_" ++ show n)

namedFieldsWith :: (Convertible a String) => (Int -> String) -> Either [b] [(a, b)] -> [(T.Text, b)]
namedFieldsWith mkName = either (zip [T.pack (mkName n) | n <- [0 :: Int ..]]) (map (first asT))

numFields :: Either [a1] [(a2, a1)] -> [T.Text]
numFields fs = [T.pack ("_" ++ show n) | n <- [1 .. length (fieldsTypes fs)]]

asT :: (Convertible a String) => a -> T.Text
asT = T.pack . convert

showT :: (Show a) => a -> T.Text
showT = asT . show

dotted, undered, commaed :: [T.Text] -> T.Text
dotted = T.intercalate "."
undered = T.intercalate "_"
commaed = T.intercalate ","

list :: p -> ([a] -> p) -> [a] -> p
list nil _ [] = nil
list _ f l = f l

obj, arr, pars, pats, sig :: [T.Text] -> T.Text
obj vs = T.concat ["{", commaed vs, "}"]
arr vs = T.concat ["[", commaed vs, "]"]
pars vs = T.concat ["(", commaed vs, ")"]
pats vs = T.concat ["(", T.unwords vs, ")"]
sig vs = T.concat ["<", commaed vs, ">"]

par :: T.Text -> T.Text
par v = T.concat ["(", v, ")"]

asFun, asFunT :: [Type T.Text] -> Type T.Text
asFun [] = error "no type"
asFun [t] = t
asFun (h : t) = TypeApp (TypeApp (TypeCon "->") h) (asFun t)
asFunT = TypeCon . T.intercalate " -> " . map (mpar . prettyT)

mpar s
  | any isSpace (T.unpack s) = T.concat ["(", s, ")"]
  | otherwise = s

snoc xs x = xs ++ [x]

newtype FlatTest a = FlatTest [a]

{-
>>> flatTests
[(TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)),"\SOH"),(TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)),"\129"),(TypeApp (TypeCon (AbsRef (SHAKE128_48 218 104 54 119 143 212))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))),"\SOH"),(TypeApp (TypeCon (AbsRef (SHAKE128_48 218 104 54 119 143 212))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))),"\129"),(TypeApp (TypeApp (TypeCon (AbsRef (SHAKE128_48 98 96 228 101 174 116))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))),"\SOH"),(TypeApp (TypeApp (TypeCon (AbsRef (SHAKE128_48 98 96 228 101 174 116))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))) (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28))),"\193")]
-}
flatTests :: (AbsEnv, [(AbsType, ByteString)])
flatTests =
  fold
    [ mkTests [False, True],
      mkTests [Nothing, Just False],
      mkTests [Left False, Right True],
      mkTests [[Left False, Right True], [Right False, Left True]],
      mkTests [(False, 3333 :: Word16)],
      mkTests [(False, 'a', True), (True, 'b', False)],
      mkTests [(False, 3333 :: Word16, True, 'a')],
      mkTests [(False, 3333 :: Word16, True, 'a', "hello" :: String)],
      mkTests [(False, 3333 :: Word16, True, 'a', "hello" :: String, 123456 :: Word)],
      mkTests [(False, 3333 :: Word16, True, 'a', "hello" :: String, False, 123456 :: Word)],
      mkTests (minMax :: [Word8]),
      mkTests (minMax :: [Word16]),
      mkTests (minMax :: [Word32]),
      mkTests (minMax :: [Word64]),
      mkTests (minMax :: [Word]),
      mkTests (minMax :: [Int8]),
      mkTests (minMax :: [Int16]),
      mkTests (minMax :: [Int32]),
      mkTests (minMax :: [Int64]),
      mkTests (minMax :: [Int]),
      mkTests [0 :: Integer, 123456789012345678901234567890123456789012345678901234567890, -123456789012345678901234567890123456789012345678901234567890],
      mkTests [0 :: Natural, 123456789012345678901234567890123456789012345678901234567890],
      mkTests ["" :: String, "a$€语"],
      mkTests ["" :: Text, "a$€语"],
      mkTests [[], [1 .. 11 :: Int]],
      mkTests "a$€语",
      mkTests "𐐷",
      mkTests [12.123 :: Float, -57.238E-11],
      mkTests [12.123 :: Double, -57.238E-11],
      mkTests [()]
    ]

-- data Unit = Unit deriving (Model, Flat)

minMax :: (Bounded a, Num a) => [a]
minMax = [1, minBound, maxBound]

mkTests :: forall a. (Flat a, Model a) => [a] -> (AbsEnv, [(AbsType, ByteString)])
mkTests as =
  let model = absTypeModel (Proxy :: Proxy a)
   in (typeEnv model, map (\v -> (typeName model, flat v)) as)
