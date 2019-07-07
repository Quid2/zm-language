{-# LANGUAGE TemplateHaskell ,CPP #-}
module FileEmbed(asTextLibrary,anyMatching,embedFiles) where
import qualified Data.ByteString as B
import Data.FileEmbed
import Language.Haskell.TH.Syntax
import Text.Regex
import Data.Text(Text)
import Data.Maybe
import qualified Data.Map as M
import Data.Bifunctor
import ZM(convert)

anyMatching :: [String] -> String -> Bool
anyMatching rexs =
    let ps = map (\r-> isJust . matchRegex (mkRegex r)) rexs
    in \s -> any  ($ s) ps

-- Read as UTF8 files    
asTextLibrary :: [(FilePath,B.ByteString)] -> M.Map FilePath Text   
asTextLibrary = M.fromList . map (second convert)

-- Code copied/adapted from package file-embed, module Data.FileEmbed

{- | Embed a directory recursively in your source code.
   
 > import qualified Data.ByteString
 >
 > myDir :: [(FilePath, Data.ByteString.ByteString)]
 > myDir = $(embedFiles [("dirName",[fileRegex])]
 > ["api.ts","core.ts","core/Array.ts","core/Bytes.ts","core/Filler.ts","core/Word7.ts","core/Word8.ts"]

 > library :: M.Map FilePath T.Text
 > library = asTextLibrary $ $(embedFiles "quid2-ts" (anyMatching ["core/[^.]+\\.ts","core\\.ts","api\\.ts"]))
 -}

embedFiles :: FilePath -> (FilePath -> Bool) ->  Q Exp
embedFiles fp predicate = do
    typ <- [t| [(FilePath, B.ByteString)] |]
    e <- ListE <$> (runIO (filter (predicate . fst) <$> getDir fp) >>= mapM (pairToExp fp))
    return $ SigE e typ

pairToExp :: FilePath -> (FilePath, B.ByteString) -> Q Exp
pairToExp _root (path, bs) = do
#if MIN_VERSION_template_haskell(2,7,0)
        qAddDependentFile $ _root ++ '/' : path
#endif
        exp' <- bsToExp bs
        return $! TupE [LitE $ StringL path, exp']

