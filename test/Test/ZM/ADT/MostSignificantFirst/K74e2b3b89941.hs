{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.MostSignificantFirst.K74e2b3b89941 (MostSignificantFirst(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Control.DeepSeq

newtype MostSignificantFirst a =   MostSignificantFirst a
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat, Control.DeepSeq.NFData)
instance ( Data.Model.Model a ) => Data.Model.Model ( MostSignificantFirst a )
