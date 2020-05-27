{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module ZM.To.PureScript
    ( WriteFlags(..)
    , Flags(..)
    , defaultPrimitiveTypes
    , generate
    , Module(..)
    , mdlFilePath
    , mdlWrite) where

import qualified Data.Map as M
import qualified Data.Text as T
import           ZM hiding (dotted, mdlName, moduleName)
import           ZM.To.Util
import           Text.PrettyPrint
import qualified Text.PrettyPrint as P

t = mapM_
  (mdlWrite
     WriteFlags { srcDir = "/Users/titto/workspace/top-apps-purs/src/"
                , overwrite = True
                })
  $ generate
    Flags { namespace = ["ZM", "ADT"]
          , primTypes = defaultPrimitiveTypes
          , addIndex = True
          }
    (absEnv p)
--
  where
    p = Proxy :: Proxy ([Float], TypedBLOB)

-- p = Proxy :: Proxy (Either Bool ())
type HFlags = Flags T.Text

defaultPrimitiveTypes :: M.Map T.Text T.Text
defaultPrimitiveTypes = M.fromList
  $ map
    (\v -> (v, v))
    ["Word8.Kb1f46a49c8f8", "Word7.Kf4c946334a7e", "Array.K2e8b4519aeaa"]

{-|
Generate a PureScript module for every absolute ADT plus, if required, an index module.

TODO: add optics for field traversal
purescript-profunctor-optics
  -}
generate :: HFlags -> AbsEnv -> [Module]
generate flags env =
  let (mdls, jmdls) = unzip $ map (generateModules flags env) (M.toList env)
      amdls = mdls ++ jmdls
  in if addIndex flags
     then generateIndex flags mdls:amdls
     else amdls

generateIndex :: HFlags -> [Module] -> Module
generateIndex flags mdls =
  let mdlNames = map mdlName mdls
      allContent = T.unlines
        (T.unwords
           ["module", dotted (namespaceT flags ++ ["All(module E) where"])]
         :map (\n -> T.unwords ["import", n, "as E"]) mdlNames)
  in pursModule (namespaceT flags ++ ["All"]) allContent

generateModules :: HFlags -> AbsEnv -> (AbsRef, AbsADT) -> (Module, Module)
generateModules flags adtEnv (adtRef, adt) =
  let ns = namespaceT flags
      -- e.g. Bit
      name = declNameT adt
      -- e.g. Bit.K65149ce3b366
      absName = moduleShortName name adtRef
      -- e.g. ZM.ADT.Bit.K65149ce3b366
      mdlName = moduleName ns name adtRef
      -- e.g. ["ZM","ADT","Bit","K65149ce3b366"]
      mdlNameC = moduleName_ (map noUnderscores) ns name adtRef
      mct :: Maybe (ConTree Identifier T.Text) = (pursRef adtEnv name <$>)
        <$> declCons adt
      -- numConstructors = maybe 0 (length . constructors) mct
      adtT :: ADT Identifier Identifier (TypeRef T.Text) =
        substAbsADT (\ref -> adtFullName adtRef (declNameS adtEnv ref) ref) adt
      vs = map T.singleton
        $ take (fromIntegral . declNumParameters $ adtT) ['a' ..]
      adtTyp :: Type T.Text = typeA
        $ TypeN (declNameT adtT) (map (\v -> TypeN v []) vs)
      -- adtTypeT = T.unwords $ declNameT adtT:vs
      cs = maybe [] constructors mct
      consSigs :: [Type T.Text] =
        map (asFunT . (`snoc` adtTyp) . fldTypes . snd) cs
      fldTypes (Left ids) = ids
      fldTypes r = [TypeCon (prettyT r)]
      decSign = asFunT (consSigs `snoc` TypeApp (TypeCon "I.Decoder") adtTyp)
      contentNormF = maybe "" foreignDecode mct
      contentPrimF = "exports.decodePRIM ="
      contentNorm = T.unlines
        $ header
        :"import Prelude(bind,pure,unit)" -- bind cannot be qualified
        :map
          (\n -> T.unwords ["import", n, "as I"])
          [ "Data.Generic.Rep (class Generic)"
          , "Data.Generic.Rep.Show(genericShow)"
          , "Prelude (class Eq, class Ord, class Show,show,bind,pure,(+),(<*>),(*>),(<>))"
          , "Flat (class Flat,getBit,putZero,putOne,maxSize,decode,encode,DecoderState,Decoder,asGet)"]
        ++ [ T.unlines
               $ map
                 (\ref -> T.unwords
                    [ "import"
                    , moduleName ns (declNameS adtEnv ref) ref
                    , "as"
                    , prettyT ref])
               $ innerReferences adt
           , T.unwords
               [ "foreign import decoder_ :: "
               , if length vs /= 0
                 then T.concat [T.unwords ("forall":vs), "."]
                 else ""
               , prettyT $ decSign]
           , prettyT
               $ prettyADT
                 (if isNewType adt
                  then "newtype "
                  else "data ")
                 '='
               $ adtT]
        ++ map
          (T.append "derive ")
          [ T.append (instanceADT "Generic" adt) " _"
          , instanceADT "Eq" adt
          , instanceADT "Ord" adt]
        -- ++ [T.append (instanceADT "Show" adt) " where show = I.genericShow"]
        ++ [showInstance]
        ++ [flatInstance]
      showInstance = maybe
        ""
        (\ct -> let cs = constructors ct
                in T.unlines
                     [ T.append (instanceADT "Show" adt) " where "
                     , tab 2 (showShow ct cs)])
        mct
      flatInstance = maybe
        ""
        (\ct
         -> let cs = constructors ct
            in T.unlines
                 [ T.append (instanceADT "Flat" adt) " where "
                 , tab 2 (flatSize ct cs ++ flatEncode ct cs ++ flatDecode cs)])
        mct
      contentPrim = T.unlines
        [headerPrim, T.concat ["import ZM.Prim(", declNameT adt, ")"]]
      headerPrim = T.unwords ["module", mdlName, "(module ZM.Prim) where"]
      header = T.unwords
        ["module", mdlName, T.concat ["(", name, "(..)", ")"], "where"]
      content = if isPrimType flags absName
                then contentPrim
                else contentNorm
      contentF = if isPrimType flags absName
                 then contentPrimF
                 else contentNormF
  in (pursModule mdlNameC content, pursForeignModule mdlNameC contentF)

tab n = let sp = T.replicate n " "
        in T.unlines . map (T.append sp)

-- flatSize :: ConTree a ref -> [T.Text]
-- flatSize :: p -> [p1] -> [T.Text]
-- flatSize :: ConTree a t -> Fields a t -> [T.Text]
flatEncode ct = map enc_
  where
    enc_ (cname, cfields) =
      let
          -- fs = numFields cfields
          (ls, rs) = lrFields cfields
          bs = map
            (\f -> if f
                   then "I.putOne"
                   else "I.putZero")
            $ maybe [] fst
            $ constructorInfo cname ct
      in T.unwords
           [ "encode"
           , pats (prettyT cname:ls)
           , "st ="
           , if either length length cfields == 0
             then "pure unit"
             else T.intercalate " I.*> "
               $ map (flip T.append " st")
               $ bs ++ map (T.append "I.encode ") rs]

showShow ct = map show_
  where
    show_ (cname, cfields) =
      let
          -- fs = numFields cfields
          (ls, rs) = lrFields cfields
          bs = map
            (\f -> if f
                   then "I.putOne"
                   else "I.putZero")
            $ maybe [] fst
            $ constructorInfo cname ct
      in T.unwords
           [ "show"
           , pats (prettyT cname:ls)
           , "="
           , T.intercalate " I.<> \" \" I.<> "
               $ (T.pack . show . T.unpack . prettyT $ cname)
               :map (T.append "I.show ") rs]

flatSize ct = map size_
  where
    size_ (cname, cfields) =
      let
          -- let (ls, rs) = either
          --       (\ff -> let fs = [T.pack ("_" ++ show n) | n <- [1 .. length ff]]
          --               in (fs, fs))
          --       (\nf -> (["v"], map (T.append "v." . asT . fst) nf))
          --       cfields
          (ls, rs) = lrFields cfields
          cl = maybe 0 (length . fst) $ constructorInfo cname ct
      in T.unwords
           [ "maxSize"
           , pats (prettyT cname:ls)
           , "="
           , T.intercalate " I.+ "
               $ T.pack (show cl):map (T.append "I.maxSize ") rs]

lrFields = either
  (\ff -> let fs = [T.pack ("_" ++ show n) | n <- [1 .. length ff]]
          in (fs, fs))
  (\nf -> (["v"], map (T.append "v." . asT . fst) nf))

-- (\bit0 bit1 bit2 bit4 -> Bits8 {bit0}}
-- flatDecode0 :: Pretty a => ConTree a ref -> [T.Text]
-- flatDecode0 ct = ["decode =", dec 2 ct]
--   where
--     dec n (ConTree l r) = tab
--       n
--       [ "do"
--       , "  b <- I.getBit"
--       , "  case b of"
--       , T.append "    false -> " $ dec (n + 2) l
--       , T.append "    true  -> " $ dec (n + 2) r]
--     dec n (Con name flds) =
--       let arity = length (fieldsTypes flds)
--           nameT = prettyT name
--           f = either
--             (const nameT)
--             (\fs
--              -> let ns = map (prettyT . fst) fs
--                 in par $ T.concat ["\\", T.unwords ns, " -> ", nameT, obj ns])
--             flds
--       in tab n [T.unwords ["I.pure", f, T.replicate arity " I.<*> I.decode"]]
-- foreignDecode :: 
foreignDecode ct =
  T.pack . render $ "exports.decoder_ = function () {throw 'NTY'}" -- P.<> dec 2 ct
  where
    dec n (ConTree l r) = nest n
      $ vcat
        [ "do"
        , "  b <- I.getBit"
        , "  case b of"
        , "    false -> " P.<> dec (n + 2) l
        , "    true  -> " P.<> dec (n + 2) r]
    dec n (Con name flds) =
      let arity = length (fieldsTypes flds)
          nameT = prettyT name
          f = either
            (const nameT)
            (\fs
             -> let ns = map (prettyT . fst) fs
                in par $ T.concat ["\\", T.unwords ns, " -> ", nameT, obj ns])
            flds
      in nest n . text . T.unpack
         $ T.unwords ["I.pure", f, T.replicate arity " I.<*> I.decode"]

flatDecode :: [(Identifier, Fields Identifier T.Text)] -> [T.Text]

-- flatDecode ct = T.lines . T.pack . render $ "decode = " P.<> dec 2 ct
-- flatDecode _ [] =  "pure $ Left \"No constructors\""
flatDecode cs =
  [ T.concat
      ["decode = I.asGet (decoder_ ", T.unwords $ map (prettyT . fst) cs, ")"]]
                                                                                          -- PS version
                                                                                          -- dec n (ConTree l r) = nest n
                                                                                          --   $ vcat
                                                                                          --     [ "do"
                                                                                          --     , "  b <- I.getBit"
                                                                                          --     , "  case b of"
                                                                                          --     , "    false -> " P.<> dec (n + 2) l
                                                                                          --     , "    true  -> " P.<> dec (n + 2) r]
                                                                                          -- dec n (Con name flds) =
                                                                                          --   let arity = length (fieldsTypes flds)
                                                                                          --       nameT = prettyT name
                                                                                          --       f = either
                                                                                          --         (const nameT)
                                                                                          --         (\fs
                                                                                          --          -> let ns = map (prettyT . fst) fs
                                                                                          --             in par $ T.concat ["\\", T.unwords ns, " -> ", nameT, obj ns])
                                                                                          --         flds
                                                                                          --   in nest n . text . T.unpack
                                                                                          --      $ T.unwords ["I.pure", f, T.replicate arity " I.<*> I.decode"]
                                                                                      -- Remove _ from module names, they are still unique because of unique ref
                                                                                      -- ZM.Type.Bit.K65149ce3b366

    where


moduleName :: Pretty a2 => [T.Text] -> T.Text -> a2 -> T.Text
moduleName = moduleName_ (noUnderscores . dotted)

-- mdlName = 
noUnderscores :: T.Text -> T.Text
noUnderscores = T.filter (/= '_')

-- K65149ce3b366.Bit
adtFullName :: (Eq a, Pretty a) => a -> T.Text -> a -> T.Text
adtFullName adtRef adtName ref
  | adtRef == ref = adtName
  | otherwise = dotted [prettyT $ ref, adtName]

instanceADT
  :: Convertible name String => T.Text -> ADT name consName ref -> T.Text
instanceADT cls adt =
  instanceCtx cls (declNameT adt) (fromIntegral . declNumParameters $ adt)

instanceCtx :: T.Text -> T.Text -> Int -> T.Text

-- instanceCtx cls@"Generic" dt _  = T.unwords [instancePrefix cls dt, icls cls, dt]
instanceCtx cls dt 0 = T.unwords [instancePrefix cls dt, icls cls, dt]
instanceCtx cls dt n =
  let vs = map T.singleton $ take n ['a' ..]
  in T.unwords
     $ instancePrefix cls dt
     :(if cls == "Generic"
       then []
       else [ "("
            , T.intercalate "," . map (\v -> T.unwords [icls cls, v]) $ vs
            , ") =>"])
     ++ [icls cls, "(", dt, T.unwords vs, ")"]

instancePrefix :: T.Text -> T.Text -> T.Text
instancePrefix cls dt = T.unwords
  ["instance", T.append (T.toLower cls) dt, "::"]

icls :: T.Text -> T.Text
icls = T.append "I."

{-
A                  -- variable
List<A>            -- self reference 
K306f1981b41c.Bool -- external reference
-}
pursRef :: (Convertible a String, Show k, Ord k, Pretty k)
        => M.Map k (ADT a consName ref)
        -> T.Text
        -> ADTRef k
        -> T.Text
pursRef _ _ (Var n) = var_ 'a' n
pursRef env _ (Ext r) = T.concat [prettyT r, ".", declNameS env r]
pursRef _ self Rec = self
