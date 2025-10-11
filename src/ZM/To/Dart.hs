{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Convert ZM ADTs to TypeScript data definitions
module ZM.To.Dart
  ( generate,
    generateModule,
    -- , library
    defaultPrimitiveTypes,
    save,
  )
where

-- import           Data.Char                      ( chr
--                                                 , ord
--                                                 , toLower
--                                                 )

-- Pretty printing

import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Posix.Internals (get_saved_termios)
import ZM
import ZM.Parser (mdl)
import ZM.To.Util

-- import           FileEmbed
-- import qualified Data.Set                      as Set
-- import           Data.Word

-- |
-- TODO:
-- - make objects into pure unmodifiable values. (no write access to fields)
-- - generate also semi-custom classes (Word/Ints,List,String..)
--   - Use js patching  to customise classes.
--
-- NOTE:
-- Constructors whose name clashes with the data type name:
-- A = A .. | ..
--
-- are rewritten as:
-- A = _A ...
--
-- \$setup

{- A simple ADT
>>> import Data.Word
>>> p0 = Proxy :: Proxy [Bool]
>>> p1 = Proxy :: Proxy AbsADT
>>> p2 = Proxy :: Proxy (BLOB Bool) -- (ByType Bit)
>>> p3 = Proxy :: Proxy (Either Bool [Bool]) -- (ByType Bit)
>>> p4 = Proxy :: Proxy Word8
>>> f1 = Flags { namespace = ["zm","adt"], primTypes = defaultPrimitiveTypes, addIndex  = False}
>>> gen p = generate f1 (absEnv p)
>>> g p = error . T.unpack .  mdlContent <$> gen p
>>> save = mapM_ (mdlWrite WriteFlags { srcDir    = "/Users/titto/workspace/flutter/notifications/lib", overwrite = True})

>>> gen (Proxy :: Proxy Bool)
[Module {mdlExtension = "dart", mdlPath = ["zm","adt","bool_k306f1981b41c"], mdlContent = "/*\nZM Type:\nBool \8801   False\n       | True\n*/\n\nimport '../../zm.dart' as zm;\nimport '../../encdec.dart' as zm;\n\nzm.ZMFold<Bool> $$Bool= <R>(zm.Folder<R> f) {return f(info_,[]);};\n\nzm.Decoder<Bool> $Bool = (zm.DecoderState st) {return Bool(st.zmBool());};\n\nconst info_ = zm.ZMTypeInfo(\n\"Bool\",\n(0x30,0x6f,0x19,0x81,0xb4,0x1c),\n);\n\n\nclass Bool  extends zm.ZM  {\n    Bool(this.value);\n    final bool value;\n    @override flatMaxSize() {return zm.EncoderState.szBool(value);}\n    @override flatEncode(zm.EncoderState st) {st.zmBool(value);}\n    @override toString() {return toStr();}\n    @override toStr({nested=false}) {return value.toString();}\n    @override pretty({nested=false}) {return value.toString();}\n\n}\n\n"}]

>>> save $ gen (Proxy :: Proxy (Either Bool Int))

>>> save $ gen (Proxy :: Proxy Filler)

>>> save $ gen (Proxy :: Proxy Text)

>>> save $ gen (Proxy :: Proxy String)

>>> save $ gen (Proxy :: Proxy (Maybe Bool))

>>> save $ gen (Proxy :: Proxy Bool)

>>> save $ gen (Proxy :: Proxy [Word8])

A primitive type
>>> save $ gen (Proxy :: Proxy Word8)

>>> save $ gen (Proxy :: Proxy Word32)

>>> gen (Proxy :: Proxy (Either Bool Bool))
[Module {mdlExtension = "dart", mdlPath = ["zm","adt","bool_k306f1981b41c"], mdlContent = "/*\nZM Type:\nBool \8801   False\n       | True\n*/\n\nimport '../../zm.dart' as zm;\nimport '../../encdec.dart' as zm;\n\nzm.ZMFold<Bool> $$Bool= <R>(zm.Folder<R> f) {return f(info_,[]);};\n\nzm.Decoder<Bool> $Bool = (zm.DecoderState st) {return Bool(st.zmBool());};\n\nconst info_ = zm.ZMTypeInfo(\n\"Bool\",\n(0x30,0x6f,0x19,0x81,0xb4,0x1c),\n);\n\n\nclass Bool  extends zm.ZM  {\n    Bool(this.value);\n    final bool value;\n    @override flatMaxSize() {return zm.EncoderState.szBool(value);}\n    @override flatEncode(zm.EncoderState st) {st.zmBool(value);}\n    @override toString() {return toStr();}\n    @override toStr({nested=false}) {return value.toString();}\n    @override pretty({nested=false}) {return value.toString();}\n\n}\n\n"},Module {mdlExtension = "dart", mdlPath = ["zm","adt","either_k6260e465ae74"], mdlContent = "/*\nZM Type:\nEither a b \8801   Left a\n             | Right b\n*/\n\nimport '../../zm.dart' as zm;\nimport '../../encdec.dart' as zm;\n\nzm.ZMFold<Either<A,B>> $$Either<A extends zm.ZM,B extends zm.ZM>(zm.ZMFold<A> t1,zm.ZMFold<B> t2) {return <R>(zm.Folder<R> f) {return f(info_,[t1(f),t2(f)]);};}\n\nzm.Decoder<Either<A,B>> $Either<A extends zm.ZM,B extends zm.ZM>(zm.Decoder<A> t1,zm.Decoder<B> t2) {return (zm.DecoderState st) {if (st.zero()) { return Left(t1(st)); } else { return Right(t2(st)); }};}\n\nconst info_ = zm.ZMTypeInfo(\n\"Either\",\n(0x62,0x60,0xe4,0x65,0xae,0x74),\n);\n\n\nsealed class Either <A extends zm.ZM,B extends zm.ZM> extends zm.ZM {\n}\n\n\nclass Left <A extends zm.ZM,B extends zm.ZM> extends Either<A,B>  {\nfinal A p0;\n\nLeft(this.p0);\n\n@override toString() {return toStr();}\n@override toStr({nested=false}) {return zm.nestedPars(nested,[\"Left\",p0.toStr(nested:true)].join(' '));}\n@override pretty({nested=false}) {return zm.nestedPars(nested,[\"Left\",p0.pretty(nested:true)].join(' '));}\n\n@override flatMaxSize() {return 1+p0.flatMaxSize();}\n@override flatEncode(zm.EncoderState st) {st.zero();p0.flatEncode(st);}\n\n}\n\nclass Right <A extends zm.ZM,B extends zm.ZM> extends Either<A,B>  {\nfinal B p0;\n\nRight(this.p0);\n\n@override toString() {return toStr();}\n@override toStr({nested=false}) {return zm.nestedPars(nested,[\"Right\",p0.toStr(nested:true)].join(' '));}\n@override pretty({nested=false}) {return zm.nestedPars(nested,[\"Right\",p0.pretty(nested:true)].join(' '));}\n\n@override flatMaxSize() {return 1+p0.flatMaxSize();}\n@override flatEncode(zm.EncoderState st) {st.one();p0.flatEncode(st);}\n\n}\n\n\n"}]
-}

-- -- x = prettyShow $ flat $ absType p4
--
-- -- t = mapM_
-- --   (mdlWrite WriteFlags { srcDir    = "/Users/titto/workspace/ts"
-- --                        , overwrite = True
-- --                        }
-- --   )
-- --   genT

-- p4 = Proxy :: Proxy RepoProtocol

t = save $ absEnv (Proxy :: Proxy (Either Bool Bool))

saveTests = mdlWrite_ $ generateTest flags_ flatTests

save :: AbsEnv -> IO ()
save = mapM_ mdlWrite_ . gen

mdlWrite_ = mdlWrite WriteFlags {srcDir = "/Users/titto/workspace/flutter/notifications/lib", overwrite = True}

flags_ = Flags {namespace = ["zm", "adt"], primTypes = defaultPrimitiveTypes, addIndex = False}

gen = generate flags_

-- TODO
reservedWords :: [Text]
reservedWords = ["String", "Function"]

-- | ZM Types that are mapped to primitive JS types
defaultPrimitiveTypes =
  M.fromList
    [ ("Word7.Kf4c946334a7e", ValueType "int"),
      ("Word8.Kb1f46a49c8f8", ValueType "int"),
      ("Word16.K295e24d62fac", ValueType "int"),
      ("Word32.K2412799c99f1", ValueType "int"),
      ("Word64.K50d018f7593a", ValueType "int"),
      ("Word.Kf92e8339908a", ValueType "int"),
      ("Int8.Kb3a2642b4a84", ValueType "int"),
      ("Int16.K3dac6bd4fa9c", ValueType "int"),
      ("Int32.K5a1fb29321a5", ValueType "int"),
      ("Int64.Kfb94cb4d4ede", ValueType "int"),
      ("Int.K102a3bb904e3", ValueType "int"),
      ("Char.K066db52af145", ValueType "String"),
      ("String.K2f006595638c", ValueType "String"),
      ("Array.K2e8b4519aeaa", ValueType "List<A>"),
      ("Bytes.Kf8844385a443", ValueType "zm.Uint8List"),
      ("Filler.Kae1dfeece189", ValueType "Null"),
      ("Bool.K306f1981b41c", ValueType "bool")
      -- ,("List.Kb8cd13187198",PrimType)
    ]

type TFlags = Flags PrimType

data PrimType = PrimType | ValueType {dartType :: T.Text} -- ,imports :: [T.Text]

-- instance Convertible ValueType String where safeConvert = Right . T.unpack . ref

-- | Generate one module for every ZM data type in the provided environment, plus an optional index module
generate :: TFlags -> AbsEnv -> [Module]
generate flags env =
  let mdls = map (generateModule flags env) (M.toList env)
   in if addIndex flags then generateIndex flags env : mdls else mdls

-- | Generate an index module
generateIndex :: TFlags -> AbsEnv -> Module
generateIndex flags env =
  dartModule ["all"] $
    imports
      (T.concat $ "./" : map (`T.snoc` '/') (namespaceT flags))
      env
      (M.keys env)

--- >>> saveTests
generateTest :: TFlags -> (AbsEnv, [(AbsType, ByteString)]) -> Module
generateTest flags (adtEnv, tests) =
  dartModule ["test"] $
    T.unlines
      ( "import 'encdec.dart';"
          : "import 'flat.dart';"
          : imports
            (T.concat $ "./" : map (`T.snoc` '/') (namespaceT flags))
            adtEnv
            (M.keys adtEnv)
          -- : "bool testEncDec(typ,bytes) => bytes.toString() == flat(unflat(typ,Uint8List.fromList(bytes))).toString();"
          : ""
          : "testAll() {"
          : map ((\ps -> "testEncDec(" <> ps <> ");") . (\(typ, enc) -> tsTypeWith pars (tyRefWith "$" adtEnv <$> typ) <> "," <> prettyT enc)) tests
          ++ ["}"]
      )

-- Does not use all flags
-- generateModule :: TFlags -> AbsEnv -> AbsRef -> Module
-- generateModuleO flags adtEnv adtRef = generateModule flags adtEnv (M.lookup )

-- dartModuleName adtName adtRef = undered $ moduleShortName_ adtName adtRef

-- dartModuleName adtName adtRef = fileRef adtRef

-- | Generate the indicated module
generateModule :: TFlags -> AbsEnv -> (AbsRef, AbsADT) -> Module
generateModule flags adtEnv (adtRef, adt) =
  let ns :: [Text] = namespaceT flags
      -- e.g. Bit
      name = declNameT adt
      -- e.g. Bit.KU65149ce3b366
      absName = moduleShortName name adtRef
      filename = mdlRef adtEnv adtRef -- dartModuleName (T.toLower name) adtRef
      mct :: Maybe (ConTree Identifier Text) = (tsRef adtEnv name <$>) <$> declCons adt
      dct :: Maybe (ConTree Identifier ([Text] -> Text)) = (decRef adtEnv name <$>) <$> declCons adt
      constrs :: [(Identifier, Fields Identifier Text)] = maybe [] constructors mct
      constrNames :: [Text] = map (asT . fst) constrs
      -- e.g. ZM.ADT.Bit.K65149ce3b366
      -- mdlName = moduleName ns name adtRef
      -- e.g. ["ZM","ADT","Bit","K65149ce3b366"]
      mdlNameC :: [Text] = ns ++ [filename]
      -- \|True if it has more than one contructor and one constructor has the same name as the adt
      -- hasNameClash = name == "String" || (length constrNames > 1 && (name `elem` constrNames))
      -- Postfix with '_' name clashing constructor
      fullConstrName :: Identifier -> Text
      fullConstrName cname =
        let n = asT cname
         in if length constrNames > 1 && n == name then T.snoc n '_' else n
      -- fullConstrNames :: [Text] = map fullConstrName constrNames
      -- fullConstrName cname = let name = asT cname in if hasNameClash then T.concat ["ZMC.",name] else name
      -- constructorsOpen = if hasNameClash then "namespace ZMC {" else ""
      -- constructorsClose = if hasNameClash then "}" else ""
      typeVars = typeVarsM adt
      adtTypeVars :: Text = typeVarsSeq " extends zm.ZM" adt
      -- sadt = substAbsADT (\ref -> adtFullName ns (declNameS adtEnv ref) ref) adt
      constrTypeVars :: Text = typeVarsSeq "" adt
      -- class Left <A extends zm.ZM,B extends zm.ZM> extends Either <A,B> {
      constrClass :: Bool -> Text -> ConTree Identifier Text -> (Identifier, Fields Identifier Text) -> Text
      constrClass isSingleton superType ct (consId, cf) =
        let nf = namedFieldsWith parName cf
            fname = fullConstrName consId
         in T.unlines
              [ classHeader isSingleton superType fname,
                -- , zmType (declNumParameters adt)
                constructor fname nf,
                toString fname nf,
                flat ct consId nf,
                "}"
              ]
      parName n = "p" ++ show n

      valueClass name valueType =
        T.unlines
          [ classHeader True "zm.ZM" name,
            indentLines
              4
              [ name <> "(this.value);",
                "final "
                  <> dartType valueType
                  <> " value;",
                "@override flatMaxSize() {return zm.EncoderState.sz"
                  <> name
                  <> "(value);}",
                "@override flatEncode(zm.EncoderState st) {st.zm" <> name <> "(value);}",
                "@override toString() {return toStr();}",
                "@override toStr({nested=false}) {return value.toString();}",
                "@override pretty({nested=false}) {return value.toString();}"
              ],
            "}"
          ]

      classHeader isSingleton superType constrName =
        let ext = if isSingleton then "zm.ZM" else superType <> constrTypeVars
         in T.unwords ["class", constrName, adtTypeVars, "extends", ext, " {"]

      typeId =
        let AbsRef (SHAKE128_48 v1 v2 v3 v4 v5 v6) = adtRef
         in pars (map h [v1, v2, v3, v4, v5, v6])
        where
          h v = T.concat ["0x", T.pack $ hex v]

      constructor _ [] = ""
      constructor cname nf =
        T.unlines
          [ -- final A p0;
            T.unlines $
              map
                (\(n, t) -> "final " <> tsType t <> " " <> n <> ";") -- T.pack $ parName (T.unpack n), ";"])
                nf,
            -- Left(this.p0);
            T.concat
              [ -- "const ",
                -- prettyT cname,
                cname,
                "(",
                T.intercalate "," $
                  map (\(n, _) -> "this." <> n) nf,
                ");"
              ]
          ]

      toString cname nf =
        let ns = map fst nf
            complex s = if null ns then "" else s
            name = T.cons '"' $ T.snoc (prettyT cname) '"'
            asStr nm =
              T.concat
                [ "@override ",
                  nm,
                  "({nested=false}) {return ",
                  complex "zm.nestedPars(nested,[",
                  name,
                  complex ",",
                  T.intercalate ","
                    . map (\n -> T.concat [n, ".", nm, "(nested:true)"])
                    $ ns,
                  complex "].join(' '))",
                  ";}\n"
                ]
         in T.concat
              [ "@override toString() {return toStr();}\n",
                asStr "toStr",
                asStr "pretty"
              ]

      flat ct cname nf =
        let ns = map fst nf
            Just (bpath, _) = constructorInfo cname ct
            maxSize =
              list ["0"] (map (\n -> T.concat [n, ".flatMaxSize()"])) ns
            flatEnc =
              T.concat $
                map ((\f -> T.concat ["st.", f, "();"]) . bool "zero" "one") bpath
                  ++ map (\n -> T.concat [n, ".flatEncode(st);"]) ns
         in T.unlines
              [ T.concat
                  [ "@override flatMaxSize() {return ",
                    T.intercalate "+" $
                      list [] ((: []) . T.pack . show . length) bpath
                        ++ maxSize,
                    ";}"
                  ],
                T.concat ["@override flatEncode(zm.EncoderState st) {", flatEnc, "}"]
              ]

      numTypeParams = declNumParameters adt

      -- const $Bit : zm.zmFold<Bit> = function (f) {return f(___,[])}
      -- const $ByType : <A extends zm.Flat>(t1:zm.zmFold<A>) => zm.zmFold<ByType<A>> = function (t1) {return function (f) {return f(___,[t1(f)])}}
      zmType = zmFoldSig <> "$$" <> name <> zmType_
      zmFoldSig = "zm.ZMFold<" <> name <> constrTypeVars <> "> "
      zmType_
        | numTypeParams == 0 = "= <R>(zm.Folder<R> f) {return " <> funcCall "f" ["info_", "[]"] <> ";};"
        | otherwise =
            T.concat
              [ adtTypeVars,
                typeVars
                  pars
                  (\(n, v) -> "zm.ZMFold<" <> v <> "> " <> tpar n),
                " {return <R>(zm.Folder<R> f) {return ", -- <R>
                funcCall "f" ["info_", arr (prepost 't' "(f)" numTypeParams)],
                ";}",
                -- " as ",zmFoldSig,
                ";}"
              ]

      typeUnflat edct =
        "zm.Decoder<" <> name <> constrTypeVars <> "> $" <> name <> adtTypeVars <> typeUnflat_ edct

      typeUnflat_ edct
        | numTypeParams == 0 = " = " <> simpleTypeUnflat edct
        | otherwise =
            T.concat
              [ typeVars pars (\(n, v) -> T.concat ["zm.Decoder<", v, "> ", tpar n]),
                " {return ",
                simpleTypeUnflat edct,
                "}"
              ]

      simpleTypeUnflat edct = "(zm.DecoderState st) {" <> unflatF_ edct <> "};"

      unflatF edct = T.unwords ["    return (st) {", unflatF_ edct, "}"]

      unflatF_ Nothing =
        T.concat ["return ", name, "(st.zm", name, pars $ map (\n -> tpar (n - 1)) [1 .. numTypeParams], ");"]
      unflatF_ (Just dct) =
        maybe
          ( T.unwords
              ["throw Error(\"", absName, "has no values and cannot be decoded\""]
          )
          dunflat
          dct

      -- lowerHead t = T.toLower T.head h : t

      -- unflat :: T.Text
      -- unflat = maybe (T.unwords ["throw Error(\"",absName,"has no values and cannot be decoded\""]) directUnflat dct

      -- directUnflat ct = T.unwords ["    return function(st) {",dunflat ct,"}"]

      -- BUG: fails for recursive defs
      -- preUnflat ct = T.unlines [decs ct
      --                           ,T.unwords ["    return function(st) {",unflat_ ct,"}"]]

      -- const decs = {"BLOB":[decoders[0],(Kf8844385a443.$Bytes)(flatDecoder)]};
      decs ct =
        T.unwords
          [ "const decs =",
            obj
              . map
                ( \(name, ts) ->
                    T.concat
                      [ "\"",
                        fullConstrName name,
                        "\":",
                        arr $ map (\t -> T.concat [decType t, "(zm.flatDecoder)"]) ts
                      ]
                )
              . filter (not . null . snd)
              . map (second fieldsTypes)
              . constructors
              $ ct,
            ";"
          ]

      dunflat (ConTree l r) =
        T.unwords ["if (st.zero()) {", dunflat l, "} else {", dunflat r, "}"]
      dunflat (Con name flds) =
        let ts = fieldsTypes flds
         in T.concat
              [ "return ",
                fullConstrName name,
                pars . map (\t -> T.concat [decType t, "(st)"]) $ ts,
                ";"
              ]

      unflat_ (ConTree l r) =
        T.unwords ["if (st.zero()) {", unflat_ l, ";} else {", unflat_ r, ";};"]
      unflat_ (Con name flds) =
        let arity = length (fieldsTypes flds)
         in T.concat
              [ if arity > 0
                  then T.concat ["const d=decs[\"", fullConstrName name, "\"]; "]
                  else "",
                "return ",
                fullConstrName name,
                pars $ prepost0 "d[" "](st)" arity
              ]

      -- otherwise = [pars $ prepost 't' ":zm.ZMFold" arity,"=> zm.ZMFold =",funcDef1 "" (prefix 't' arity) (funcDef1 "" ["f"] (funcCall "f" ["___",arr (prepost 't' "(f)" arity)]))]

      typeInfo =
        T.unlines
          [ "const info_ = zm.ZMTypeInfo(",
            T.pack $ show name <> ",",
            typeId <> ",",
            -- "  (decoders) {",
            -- indent 4 (unflatF edct),
            -- "  }",
            ");"
          ]

      remark =
        T.unlines ["/*", "ZM Type:", T.pack $ take 1000 $ prettyShow adt, "*/"]

      contentHeader ::
        Maybe (Maybe (ConTree Identifier ([T.Text] -> T.Text))) -> T.Text
      contentHeader edct =
        T.unlines
          [ remark,
            "import '../../zm.dart' as zm;",
            "import '../../encdec.dart' as zm;",
            -- imports "./" adtEnv $ innerReferences adt,
            extRefs edct,
            zmType,
            "",
            typeUnflat edct,
            "",
            typeInfo
          ]

      extRefs Nothing = ""
      extRefs (Just _) = imports "./" adtEnv $ innerReferences adt

      contentValue vt =
        T.unlines
          [ contentHeader Nothing,
            valueClass name vt
          ]

      contentNorm ::
        Maybe (ConTree Identifier T.Text) ->
        Maybe (Maybe (ConTree Identifier ([T.Text] -> T.Text))) ->
        T.Text
      contentNorm mct edct =
        T.unlines
          [ contentHeader edct,
            maybe
              -- No constructors, e.g. data Void
              (T.unwords ["class", name, "extends zm.ZM {}"])
              ( \ct ->
                  let cs = constructors ct
                      unionType =
                        T.unlines
                          [ T.unwords
                              [ "sealed class",
                                name,
                                adtTypeVars,
                                "extends zm.ZM {"
                              ],
                            "}"
                          ]
                   in T.unlines $
                        case cs of
                          [consInfo] -> [constrClass True name ct consInfo]
                          _ -> unionType : "" : map (constrClass False name ct) cs
              )
              mct
          ]
      -- contentPrim = T.unwords ["{", commaed (declNameT adt : fullConstrNames), "} from '../../core'"]
      -- contentPrim = T.concat ["* from '",libFile "core","/",declNameT adt,"'"]
      contentPrim =
        T.concat ["* from '", "../../lib/core", "/", declNameT adt, "'"]

      content = case primType flags absName of
        Just vt@(ValueType _) -> contentValue vt
        Just PrimType -> contentPrim
        Nothing -> contentNorm mct (Just dct)
   in dartModule mdlNameC content

-- libFile = T.append "./lib/"

-- import * as K306f1981b41c from '<dir>Bool/K306f1981b41c'
imports :: Text -> AbsEnv -> [AbsRef] -> Text
imports dir adtEnv =
  semis
    . map
      ( \ref ->
          T.unwords
            [ "import",
              T.concat ["'", dir, fileRef adtEnv ref, "'"],
              "as ",
              mdlRef adtEnv ref
            ]
      )

semis = T.unlines . map semi

semi = (`T.snoc` ';')

-- Return the corresponding Dart type
tsType :: (Pretty a) => Type a -> T.Text
tsType = tsTypeWith sig

tsTypeWith pars = pt . typeN
  where
    pt (TypeN f []) = pr f
    pt (TypeN f ps) = T.concat [pr f, pars (map pt ps)]
    pr = T.pack . prettyShow

-- decType :: Pretty a => Type a -> T.Text
decType :: Type ([c] -> c) -> c
decType = pt . typeN
  where
    -- pt (TypeN f []) = pr f
    pt (TypeN f ps) = f (map pt ps) -- T.concat [pr f, pars (map pt ps)]
    pr = T.pack . prettyShow

-- Type decoder reference
-- decRef _ _ (Var n) = \[] -> T.concat ["decoders[", showT $ n+1, "]"]
decRef :: AbsEnv -> Text -> ADTRef AbsRef -> ([Text] -> Text)
decRef _ _ (Var n) = \[] -> tpar n
decRef env _ (Ext r) = \ds -> T.concat [tyRefWith "$" env r, params ds]
decRef _ self Rec = \ds -> T.concat ["$", self, params ds]

params [] = ""
params ds = par . commaed $ ds

tpar n = "t" <> showT (n + 1)

{-
A                       -- variable
List<A>                 -- self reference
bool_k306f1981b41c.Bool -- external reference
-}

tsRef :: AbsEnv -> Text -> ADTRef AbsRef -> Text
tsRef _ _ (Var n) = var n
tsRef env _ (Ext r) = tyRef env r -- T.concat [prettyT r, ".", declNameS env r]
tsRef _ self Rec = self

tyRef :: AbsEnv -> AbsRef -> Text
tyRef = tyRefWith ""

tyRefWith :: Text -> AbsEnv -> AbsRef -> Text
tyRefWith prefix adtEnv ref = mdlRef adtEnv ref <> "." <> prefix <> declNameS adtEnv ref

fileRef :: AbsEnv -> AbsRef -> Text
fileRef adtEnv ref = mdlRef adtEnv ref <> ".dart"

mdlRef adtEnv ref = let name = declNameS adtEnv ref in T.toLower (name <> "_" <> prettyT ref)

typeVars :: ADT name consName ref -> [T.Text]
typeVars adt = map (var . (\v -> v - 1)) [1 .. declNumParameters adt]

typeVarsSeq :: T.Text -> ADT name consName ref -> T.Text
typeVarsSeq post = list "" (sig . map (`T.append` post)) . typeVars

-- typeSigM f = list "" (sig . map f) . zip [0..] . typeVars

-- typeVarsM :: (Num a, Enum a) =>
--                ADT name consName ref -> ([b] -> String) -> ((a, T.Text) -> b) -> T.Text
typeVarsM adt lst f = list "" (lst . map f) . zip [0 ..] $ typeVars adt

funcAnon = funcDef1 ""

funcDef1 :: Text -> [Text] -> Text -> Text
funcDef1 name vs body = funcDef name vs $ T.unwords ["return", body]

funcDef name vs body = T.concat [" ", name, pars vs, " {", body, "}"]

funcCall name vs = T.concat [name, pars vs]

prefix :: (Show a, Num a, Enum a) => Char -> a -> [T.Text]
prefix pre = prepost pre ""

-- inx f vs = map (\v -> f T.concat[pre `T.cons` T.pack (show n),post]) vs

prepost ::
  (Show a, Num a, Enum a) =>
  Char ->
  T.Text ->
  a ->
  [T.Text]
prepost pre post n =
  map (\n -> T.concat [pre `T.cons` T.pack (show n), post]) [1 .. n]

prepost0 ::
  (Show a, Num a, Enum a) =>
  T.Text ->
  T.Text ->
  a ->
  [T.Text]
prepost0 pre post n =
  map (\n -> T.concat [pre `T.append` T.pack (show n), post]) [0 .. n - 1]

indent :: Int -> T.Text -> T.Text
indent n = T.append (T.replicate n " ")

indentLines :: Int -> [Text] -> Text
indentLines n = T.unlines . map (indent n)
