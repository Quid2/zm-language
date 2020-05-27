{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.NonEmptyList.Kbf2d1c86eb20 (NonEmptyList(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Control.DeepSeq

data NonEmptyList a =   Elem a
                      | Cons a (Test.ZM.ADT.NonEmptyList.Kbf2d1c86eb20.NonEmptyList a)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat, Control.DeepSeq.NFData)
instance ( Data.Model.Model a ) => Data.Model.Model ( NonEmptyList a )
