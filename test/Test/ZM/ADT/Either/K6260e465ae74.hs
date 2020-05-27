{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Either.K6260e465ae74 (Either(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Control.DeepSeq

data Either a b =   Left a
                  | Right b
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat, Control.DeepSeq.NFData)
instance ( Data.Model.Model a,Data.Model.Model b ) => Data.Model.Model ( Either a b )
