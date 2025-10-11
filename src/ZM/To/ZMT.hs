{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- Convert to ZM textual representation -}
module ZM.To.ZMT
  ( generate,
  )
where

import Data.List.Unique (unique)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import ZM
import ZM.To.Util (moduleShortName)

{-
Display a set of absolute data types, either fully or minimally qualified.

>>> import qualified Data.Text.IO as T
>>> sh = error . ("\n" ++) . T.unpack

>>> sh $ generate False $ absEnv (Proxy :: Proxy [Bool])
Bool ≡   False
       | True;
List a ≡   Nil
         | Cons a (List a)

>>> sh $ generate True $ absEnv (Proxy :: Proxy [Bool])
Bool.K306f1981b41c ≡   False
                     | True;
List.Kb8cd13187198 a ≡   Nil
                       | Cons a (List.Kb8cd13187198 a)

prop> \(w::Word8) -> w == w
Add QuickCheck to your cabal dependencies to run this test.

-}
generate :: Bool -> AbsEnv -> T.Text
generate fullyQualified absEnv =
  T.intercalate ";\n" . map (T.pack . prettyShow . gadt) . M.assocs $ absEnv
  where
    gadt (ref, adt) =
      let sadt = substAbsADT rname adt
       in ADT (fname sadt ref) (declNumParameters adt) (declCons sadt)
    rname ref = fname (solve ref absEnv) ref
    fname adt ref =
      let n = asT $ declName adt
       in if not fullyQualified && S.member n uniqueNames
            then n
            else moduleShortName n ref
    asT :: Identifier -> T.Text
    asT = T.pack . convert
    uniqueNames = S.fromList . unique . map (asT . declName) $ M.elems absEnv

-- in  convert . vspacedP . map (pPrint . substAbsADT name) $ adts
