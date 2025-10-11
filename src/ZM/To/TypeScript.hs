{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}

{-| Convert ZM ADTs to TypeScript data definitions  -}
module ZM.To.TypeScript
  ( generate
  , generateModule
  -- , library
  , defaultPrimitiveTypes
  )
where

import           Data.Bifunctor
import           Data.Bool
-- import           Data.Char                      ( chr
--                                                 , ord
--                                                 , toLower
--                                                 )
import qualified Data.Map       as M
import qualified Data.Text      as T
import           ZM
import           ZM.To.Util
-- import           FileEmbed
-- import qualified Data.Set                      as Set
-- import           Data.Word


{- |
TODO:
- make objects into pure unmodifiable values. (no write access to fields)
- generate also semi-custom classes (Word/Ints,List,String..)
  - Use js patching  to customise classes.

NOTE:
Constructors whose name clashes with the data type name:
A = A .. | ..

are rewritten as:
A = _A ...

$setup
>>> import Data.Word
>>> p0 = Proxy :: Proxy [Bool]
>>> p1 = Proxy :: Proxy AbsADT
>>> p2 = Proxy :: Proxy (BLOB Bool) -- (ByType Bit)
>>> p3 = Proxy :: Proxy (Either Bool [Bool]) -- (ByType Bit)
>>> p4 = Proxy :: Proxy Word8
>>> f1 = Flags { namespace = ["ADT"], primTypes = defaultPrimitiveTypes, addIndex  = False}
>>> gen p = generate f1 (absEnv p)

A simple ADT
>>> gen (Proxy :: Proxy Bool)
[Module {mdlExtension = "ts", mdlPath = ["ADT","Bool","K306f1981b41c"], mdlContent = "/** ZM Type:\nBool \8801   False\n       | True\n*/\n\nimport * as Q from '@quid2/core'\n\nexport const $Bool:Q.zmFold<Bool> = function (f) {return f(___,[])}\n\nexport const ___ : Q.zmTypeInfo = {\n  zid : [0x30,0x6f,0x19,0x81,0xb4,0x1c],\n  decoder : function (decoders) {\n        return function(st) { if (st.zero()) { return new False() } else { return new True() } }\n  }\n}\n\n\nexport type Bool  = False  | True \n\nexport class False  implements Q.ZM {\n\n  toString():string {return this.toStr(false)}\n  toStr(nested=false):string {return \"False\"}\n  pretty(nested=false):string {return \"False\"}\n\n  match <R>(m:{False:R,True:R}) : R {return m.False;}\n  flatMaxSize():number {return 1+0;}\n  flatEncode(st:Q.EncoderState) {st.zero();}\n\n}\n\nexport class True  implements Q.ZM {\n\n  toString():string {return this.toStr(false)}\n  toStr(nested=false):string {return \"True\"}\n  pretty(nested=false):string {return \"True\"}\n\n  match <R>(m:{False:R,True:R}) : R {return m.True;}\n  flatMaxSize():number {return 1+0;}\n  flatEncode(st:Q.EncoderState) {st.one();}\n\n}\n\n\n"}]


A primitive type
>>> gen (Proxy :: Proxy Word8)

-- x = prettyShow $ flat $ absType p4

-- t = mapM_
--   (mdlWrite WriteFlags { srcDir    = "/Users/titto/workspace/ts"
--                        , overwrite = True
--                        }
--   )
--   genT

-}

--p4 = Proxy :: Proxy RepoProtocol

-- |ZM Types that are mapped to primitive JS types
defaultPrimitiveTypes = M.fromList
  [ ("Word8.Kb1f46a49c8f8" , ValueType "number")
  , ("Word7.Kf4c946334a7e" , ValueType "number")
  , ("Word16.K295e24d62fac", ValueType "number")
  , ("Word32.K2412799c99f1", ValueType "number")
  , ("Char.K066db52af145"  , ValueType "string")
  , ("Array.K2e8b4519aeaa" , ValueType "A[]")
  , ("Bytes.Kf8844385a443" , ValueType "Uint8Array")
  , ("String.K2f006595638c", ValueType "string")
  , ("Filler.Kae1dfeece189", ValueType "string")
  --,("List.Kb8cd13187198",PrimType)
  ]

type TFlags = Flags PrimType

data PrimType = PrimType | ValueType {jsType::T.Text}

-- instance Convertible ValueType String where safeConvert = Right . T.unpack . ref

-- |Generate one module for every ZM data type in the provided environment, plus an optional index module
generate :: TFlags -> AbsEnv -> [Module]
generate flags env =
  let mdls = map (generateModule flags env) (M.toList env)
  in  if addIndex flags then generateIndex flags env : mdls else mdls

-- |Generate an index module
generateIndex :: TFlags -> AbsEnv -> Module
generateIndex flags env = tsModule ["All"] $ imports
  (T.concat $ "./" : map (`T.snoc` '/') (namespaceT flags))
  env
  (M.keys env)

-- Does not use all flags
-- generateModule :: TFlags -> AbsEnv -> AbsRef -> Module
-- generateModuleO flags adtEnv adtRef = generateModule flags adtEnv (M.lookup )

-- |Generate the indicated module
generateModule :: TFlags -> AbsEnv -> (AbsRef, AbsADT) -> Module
generateModule flags adtEnv (adtRef, adt) =
  let
    ns           = namespaceT flags
    -- e.g. Bit
    name         = declNameT adt
    -- e.g. Bit.K65149ce3b366
    absName      = moduleShortName name adtRef
    mct          = (tsRef adtEnv name <$>) <$> declCons adt
    dct          = (decRef adtEnv name <$>) <$> declCons adt
    constrs      = maybe [] constructors mct
    constrNames  = map (asT . fst) constrs
    -- e.g. ZM.ADT.Bit.K65149ce3b366
    --mdlName = moduleName ns name adtRef
    -- e.g. ["ZM","ADT","Bit","K65149ce3b366"]
    mdlNameC     = moduleName_ id ns name adtRef
    -- |True if it has more than one contructor and one constructor has the same name as the adt
    hasNameClash = length constrNames > 1 && (name `elem` constrNames)
    -- Prefix with '_' name clashing constructor
    fullConstrName cname =
      let n = asT cname
      in  if hasNameClash && name == n then T.cons '_' n else n
    fullConstrNames = map fullConstrName constrNames
    -- fullConstrName cname = let name = asT cname in if hasNameClash then T.concat ["ZMC.",name] else name
    --constructorsOpen = if hasNameClash then "export namespace ZMC {" else ""
    --constructorsClose = if hasNameClash then "}" else ""
    typeVars        = typeVarsM adt
    adtTypeVars     = typeVarsSeq " extends Q.ZM" adt
    --sadt = substAbsADT (\ref -> adtFullName ns (declNameS adtEnv ref) ref) adt
    constrTypeVars  = typeVarsSeq "" adt
    -- e.g. class Cons<A extends Q.Flat> implements Q.Flat {}
    constrClass
      :: ConTree Identifier T.Text
      -> (Identifier, Fields Identifier T.Text)
      -> T.Text
    constrClass ct (cname, cf) =
      let nf = namedFields cf
          cs = constructors ct
      in  T.unlines
            [ classHeader (fullConstrName cname)
           --, zmType (declNumParameters adt)
            , constructor nf
            , toString cname nf
            , match cs cname nf
            , flat ct cname nf
            , "}"
            ]

    valueClass name valueType = T.unlines
      [ classHeader name
      , T.unlines $ map
        (indent 4)
        [ T.concat
          ["constructor(public readonly value:", jsType valueType, ") { }"]
        , T.concat
          ["flatMaxSize() {return Q.EncoderState.sz", name, "(this.value);}"]
        , T.concat
          ["flatEncode(st: Q.EncoderState) {st.zm", name, "(this.value);}"]
        , "toString(): string {return this.toStr(false)}"
        , "toStr(nested?:boolean): string {return this.value.toString();}"
        , "pretty(nested?:boolean): string {return this.toString(); }"
        ]
      , "}"
      ]

    classHeader name =
      T.unwords ["export class", name, adtTypeVars, "implements Q.ZM {"]

    -- match <R>(m:{Nothing:R,Just:(v:A)=>R}) : R {return m.Nothing;}
    -- match <R>(m:{Nothing:R,Just:(v:A)=>R}) : R {return m.Just(this._0);}
    match cs cname nf = T.concat
      [ "  match <R>(m:"
      , matchSig cs
      , ") : R {return m."
      , asT cname
      , if null nf then "" else pars (map (T.append "this." . fst) nf)
      , ";}"
      ]
    -- Nothing:R,Just:(v:A) =>R
    matchSig =
      obj
        . map
            (\(name, flds) ->
              T.concat [asT name, ":", matchConsSig (fieldsTypes flds)]
            )
    matchConsSig = list
      "R"
      ( (`T.append` "=>R")
      . pars
      . map (\(n, t) -> T.concat ["v", T.pack $ show n, ":", tsType t])
      . zip [0 ..]
      )
    typeId =
      let AbsRef (SHAKE128_48 v1 v2 v3 v4 v5 v6) = adtRef
      in  arr (map h [v1, v2, v3, v4, v5, v6])
      where h v = T.concat ["0x", T.pack $ hex v]
    -- Alternative: defined as singleton:
    -- private static instance:<T>;constructor() {return this._instance || (this._instance = new this())}
    constructor [] = ""
    constructor nf = T.unlines
      [ "  constructor("
      , T.unlines $ map
        (\(n, t) -> T.concat ["    public readonly ", n, ": ", tsType t, ","])
        nf
      , "  ) { }"
      ]
    toString cname nf =
      let
        ns = map fst nf
        complex s = if null ns then "" else s
        name = T.cons '"' $ T.snoc (prettyT cname) '"'
        asStr nm = T.concat
          [ "  "
          , nm
          , "(nested=false):string {return "
          , complex "Q.nestedPars(nested,["
          , name
          , complex ","
          , T.intercalate ","
          . map (\n -> T.concat ["this.", n, ".", nm, "(true)"])
          $ ns
          , complex "].join(' '))"
          , "}\n"
          ]
      in
        T.concat
          [ "  toString():string {return this.toStr(false)}\n"
          , asStr "toStr"
          , asStr "pretty"
          ]

    flat ct cname nf =
      let
        ns              = map fst nf
        Just (bpath, _) = constructorInfo cname ct
        maxSize =
          list ["0"] (map (\n -> T.concat ["this.", n, ".flatMaxSize()"])) ns
        flatEnc =
          T.concat
            $ map ((\f -> T.concat ["st.", f, "();"]) . bool "zero" "one") bpath
            ++ map (\n -> T.concat ["this.", n, ".flatEncode(st);"]) ns
      in
        T.unlines
          [ T.concat
            [ "  flatMaxSize():number {return "
            , T.intercalate "+"
            $  list [] ((: []) . T.pack . show . length) bpath
            ++ maxSize
            , ";}"
            ]
          , if T.null flatEnc
            then "  flatEncode() {}"
            else T.concat ["  flatEncode(st:Q.EncoderState) {", flatEnc, "}"]
          ]

    -- always import from quid2-core
    importLib as _ = T.concat
      [ "import * as "
      , as
      , " from '@quid2/core'"
      ]

    -- always import locally
    -- importLib as name = T.concat
    --   [ "import * as "
    --   , as
    --   , " from '"
    --   , T.concat $ replicate (length ns + 1) "../"
    --   , libFile name
    --   , "'"
    --   ]
    -- libFile = T.append "@quid2/core"

    -- THis can be either a local reference (in quid2), e.g.: "'../.." or an external reference to the quid2 lib
    -- importLib as name = T.concat
    --   [ "import * as "
    --   , as
    --   , " from '"
    --   , T.concat $ replicate (length ns + 1) "../"
    --   , libFile name
    --   , "'"
    --   ]
    arity = declNumParameters adt
    -- export function zmType(f:zmFold) {return f([0x4b,0xbd,0x38,0x58,0x7b,0x9e],[]);}
    -- export function zmType(t1:any) {return function(f:zmFold) {f([0x22,0xbd,0x38,0x58,0x7b,0x9e],[t1(f)])}}

    unflatF edct = T.unwords ["    return function(st) {", unflatF_ edct, "}"]

    unflatF_ Nothing =
      T.concat ["return new ", name, "(st.zm", name, "(decoders));"]
    --unflatF_ (Right dct) = maybe (T.unwords ["throw Error(\"",absName,"has no values and cannot be decoded\""]) (\ct -> T.unwords ["    return function(st) {",dunflat ct,"}"]) dct
    unflatF_ (Just dct) = maybe
      (T.unwords
        ["throw Error(\"", absName, "has no values and cannot be decoded\""]
      )
      dunflat
      dct

    --lowerHead t = T.toLower T.head h : t

    -- unflat :: T.Text
    -- unflat = maybe (T.unwords ["throw Error(\"",absName,"has no values and cannot be decoded\""]) directUnflat dct

    -- directUnflat ct = T.unwords ["    return function(st) {",dunflat ct,"}"]

    -- BUG: fails for recursive defs
    -- preUnflat ct = T.unlines [decs ct
    --                           ,T.unwords ["    return function(st) {",unflat_ ct,"}"]]

    -- const decs = {"BLOB":[decoders[0],(Kf8844385a443.$Bytes)(flatDecoder)]};
    decs ct = T.unwords
      [ "const decs ="
      , obj
      . map
          (\(name, ts) -> T.concat
            [ "\""
            , fullConstrName name
            , "\":"
            , arr $ map (\t -> T.concat [decType t, "(Q.flatDecoder)"]) ts
            ]
          )
      . filter (not . null . snd)
      . map (second fieldsTypes)
      . constructors
      $ ct
      , ";"
      ]

    dunflat (ConTree l r) =
      T.unwords ["if (st.zero()) {", dunflat l, "} else {", dunflat r, "}"]
    dunflat (Con name flds) =
      let ts = fieldsTypes flds
      in  T.concat
            [ "return new "
            , fullConstrName name
            , pars . map (\t -> T.concat [decType t, "(st)"]) $ ts
            ]

    unflat_ (ConTree l r) =
      T.unwords ["if (st.zero()) {", unflat_ l, "} else {", unflat_ r, "}"]
    unflat_ (Con name flds) =
      let arity = length (fieldsTypes flds)
      in  T.concat
            [ if arity > 0
              then T.concat ["const d=decs[\"", fullConstrName name, "\"]; "]
              else ""
            , "return new "
            , fullConstrName name
            , pars $ prepost0 "d[" "](st)" arity
            ]

    -- export const $Bit : Q.zmFold<Bit> = function (f) {return f(___,[])}
    -- export const $ByType : <A extends Q.Flat>(t1:Q.zmFold<A>) => Q.zmFold<ByType<A>> = function (t1) {return function (f) {return f(___,[t1(f)])}}
    zmType = T.concat $ ["export const ", T.cons '$' name, ":"] ++ zmType_
    zmType_
      | arity == 0
      = [ "Q.zmFold<"
        , name
        , "> = "
        , funcDef1 "" ["f"] (funcCall "f" ["___", "[]"])
        ]
      | otherwise
      = [ adtTypeVars
        , typeVars pars
                   (\(n, v) -> T.concat ["t", showT n, ":Q.zmFold<", v, ">"])
        , " => Q.zmFold<"
        , name
        , typeVars sig snd
        , "> = "
        , funcDef1
          ""
          (prefix 't' arity)
          (funcDef1 ""
                    ["f"]
                    (funcCall "f" ["___", arr (prepost 't' "(f)" arity)])
          )
        ]
            -- otherwise = [pars $ prepost 't' ":Q.zmFold" arity,"=> Q.zmFold =",funcDef1 "" (prefix 't' arity) (funcDef1 "" ["f"] (funcCall "f" ["___",arr (prepost 't' "(f)" arity)]))]


    typeInfo edct = T.unlines
      [ "export const ___ : Q.zmTypeInfo = {"
      , T.concat ["  zid : ", typeId, ","]
      , "  decoder : function (decoders) {"
      , indent 4 (unflatF edct)
      , "  }"
      , "}"
      ]

    remark =
      T.unlines ["/** ZM Type:", T.pack $ take 1000 $ prettyShow adt, "*/"]

    contentHeader
      :: Maybe (Maybe (ConTree Identifier ([T.Text] -> T.Text))) -> T.Text
    contentHeader edct = T.unlines
      [ remark
      , importLib "Q" "core"
          --,importLib "ZM" "zm"
          -- import * as K306f1981b41c from '../Bool/K306f1981b41c'
      , imports "../" adtEnv $ innerReferences adt
      , zmType
      , ""
      , typeInfo edct
      ]

    contentValue vt = T.unlines [contentHeader Nothing, valueClass name vt]

    contentNorm
      :: Maybe (ConTree Identifier T.Text)
      -> Maybe (Maybe (ConTree Identifier ([T.Text] -> T.Text)))
      -> T.Text
    contentNorm mct edct = T.unlines
      [ contentHeader edct
        -- [remark
        -- ,importLib "Q" "core"
        -- --,importLib "ZM" "zm"
        -- -- import * as K306f1981b41c from '../Bool/K306f1981b41c'
        -- , imports "../" adtEnv $ innerReferences adt
        -- , zmType
        -- , ""
        -- ,typeInfo edct
        -- e.g. type List<T extends Q.Flat> = Nil<T> | Cons<T>
      , maybe
        (T.unwords ["export class", name, "{}"])
        (\ct ->
          let
            cs        = constructors ct
            unionType = T.unwords
              [ "export type"
              , name
              , adtTypeVars
              , "="
              , T.intercalate
                " | "
                (map
                  (\(cname, _) ->
                    T.unwords [fullConstrName cname, constrTypeVars]
                  )
                  cs
                )
              ]
            constrClasses = map (constrClass ct) cs
          in
            T.unlines $ if length cs > 1
              then unionType : "" : constrClasses
              else constrClasses
        )
        mct
      ]
    --contentPrim = T.unwords ["export {", commaed (declNameT adt : fullConstrNames), "} from '../../core'"]
    --contentPrim = T.concat ["export * from '",libFile "core","/",declNameT adt,"'"]
    contentPrim =
      T.concat ["export * from '", "../../lib/core", "/", declNameT adt, "'"]

    content = case primType flags absName of
      Just vt@(ValueType _) -> contentValue vt
      Just PrimType         -> contentPrim
      Nothing               -> contentNorm mct (Just dct)
  in
    tsModule mdlNameC content

-- libFile = T.append "./lib/"


-- import * as K306f1981b41c from '<dir>Bool/K306f1981b41c'
imports
  :: (Convertible a String, Show a1, Ord a1, Pretty a1)
  => T.Text
  -> M.Map a1 (ADT a consName ref)
  -> [a1]
  -> T.Text
imports dir adtEnv = T.unlines . map
  (\ref -> T.unwords
    [ "import * as"
    , prettyT ref
    , "from "
    , T.concat ["'", dir, declNameS adtEnv ref, "/", prettyT ref, "'"]
    ]
  )

-- Return the corresponding Typescript type
tsType :: Pretty a => Type a -> T.Text
tsType = pt . typeN
 where
  pt (TypeN f []) = pr f
  pt (TypeN f ps) = T.concat [pr f, sig (map pt ps)]
  pr = T.pack . prettyShow

-- decType :: Pretty a => Type a -> T.Text
decType = pt . typeN
 where
        --pt (TypeN f []) = pr f
  pt (TypeN f ps) = f (map pt ps) -- T.concat [pr f, pars (map pt ps)]
  pr = T.pack . prettyShow

-- Type decoder reference
decRef _ _ (Var n) = \[] -> T.concat ["decoders[", showT n, "]"]
decRef env _ (Ext r) =
  \ds -> T.concat [prettyT r, ".___.decoder", par . arr $ ds]
decRef _ self Rec = \ds -> T.concat ["___.decoder", par . arr $ ds]

{-
A                  -- variable
List<A>            -- self reference
K306f1981b41c.Bool -- external reference
-}
tsRef
  :: (Convertible a String, Show k, Ord k, Pretty k)
  => M.Map k (ADT a consName ref)
  -> T.Text
  -> ADTRef k
  -> T.Text
tsRef _   _    (Var n) = var n
tsRef env _    (Ext r) = T.concat [prettyT r, ".", declNameS env r]
tsRef _   self Rec     = self


-- decRef _ _ (Var n) = T.concat["Q.zmConst(decoders[",showT n,"])"]
-- decRef env _ (Ext r) = T.concat [prettyT r, ".$", declNameS env r]
-- decRef _ self Rec = T.cons '$' self

typeVars :: ADT name consName ref -> [T.Text]
typeVars adt = map (var . (\v -> v - 1)) [1 .. declNumParameters adt]

typeVarsSeq :: T.Text -> ADT name consName ref -> T.Text
typeVarsSeq post = list "" (sig . map (`T.append` post)) . typeVars

--typeSigM f = list "" (sig . map f) . zip [0..] . typeVars

-- typeVarsM :: (Num a, Enum a) =>
--                ADT name consName ref -> ([b] -> String) -> ((a, T.Text) -> b) -> T.Text
typeVarsM adt lst f = list "" (lst . map f) . zip [0 ..] $ typeVars adt

funcDef1 name vs body = funcDef name vs $ T.unwords ["return", body]

funcDef name vs body = T.concat ["function ", name, pars vs, " {", body, "}"]

funcCall name vs = T.concat [name, pars vs]




prefix :: (Show a, Num a, Enum a) => Char -> a -> [T.Text]
prefix pre = prepost pre ""

--inx f vs = map (\v -> f T.concat[pre `T.cons` T.pack (show n),post]) vs

prepost :: (Show a, Num a, Enum a) =>
             Char -> T.Text -> a -> [T.Text]
prepost pre post n =
  map (\n -> T.concat [pre `T.cons` T.pack (show n), post]) [1 .. n]

prepost0 :: (Show a, Num a, Enum a) =>
              T.Text -> T.Text -> a -> [T.Text]
prepost0 pre post n =
  map (\n -> T.concat [pre `T.append` T.pack (show n), post]) [0 .. n - 1]

indent :: Int -> T.Text -> T.Text
indent n = T.append (T.replicate n " ")

-- Is this still required?
-- library :: M.Map FilePath T.Text
-- library =
--   asTextLibrary
--     $ $(embedFiles "quid2-ts" (anyMatching ["core/[^.]+\\.ts","core\\.ts","api\\.ts"]))

{-
export abstract class Maybe_ <A extends Q.Flat> {
  static zid : [0xda,0x68,0x36,0x77,0x8f,0xd4];

  static decoder : function (decoders:any[]) {
    return function(st:any) {
            if (st.bit()) {return new Just(decoders[0](st));}
            else return new Nothing();
    }
  };
}
-}
