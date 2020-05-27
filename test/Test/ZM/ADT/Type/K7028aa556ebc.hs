{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.Type.K7028aa556ebc (Type(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Control.DeepSeq

data Type a =   TypeCon a
              | TypeApp (Test.ZM.ADT.Type.K7028aa556ebc.Type a)
                        (Test.ZM.ADT.Type.K7028aa556ebc.Type a)
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat, Control.DeepSeq.NFData)
instance ( Data.Model.Model a ) => Data.Model.Model ( Type a )
