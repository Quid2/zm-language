{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Maybe.Kda6836778fd4 (Maybe(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Control.DeepSeq

data Maybe a =   Nothing
               | Just a
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat, Control.DeepSeq.NFData)
instance ( Data.Model.Model a ) => Data.Model.Model ( Maybe a )
