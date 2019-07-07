module Main where
import           Data.List                      ( isSuffixOf )
--import           Debug.Trace
import           System.FilePath.Find
import           Test.DocTest

main :: IO ()
main = do
  fs <- find
    always
    (   (extension ==? ".hs")
    &&? exceptFiles ["Test.hs", "Data/Timeless.hs"]
    )
    "src"
  doctest $ ["-package megaparsec-6.5.0","-package QuickCheck","--fast"] ++ fs

exceptFiles :: Foldable t => t String -> FindClause Bool
exceptFiles mdls =
  -- let excludes = liftOp (\fp mdls -> not $ any (\mdl -> isSuffixOf mdl (traceShowId fp)) mdls)
  let excludes = liftOp (\fp mdls -> not $ any (`isSuffixOf` fp) mdls)
  in  filePath `excludes` mdls
