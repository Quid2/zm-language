{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Test.ZM.ADT.UnicodeLetterOrNumberOrLine.K33445520c45a (UnicodeLetterOrNumberOrLine(..)) where
import qualified Prelude(Eq,Ord,Show)
import qualified GHC.Generics
import qualified Flat
import qualified Data.Model
import qualified Control.DeepSeq
import qualified Test.ZM.ADT.Char.K066db52af145

newtype UnicodeLetterOrNumberOrLine =   UnicodeLetterOrNumberOrLine Test.ZM.ADT.Char.K066db52af145.Char
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show, GHC.Generics.Generic, Flat.Flat, Control.DeepSeq.NFData)
instance Data.Model.Model UnicodeLetterOrNumberOrLine
