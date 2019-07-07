module ToTypeScriptTest where

import ZM    
import ZM.To.TypeScript
import ZM.To.Util

p0 = Proxy :: Proxy Bool
p1 = Proxy :: Proxy AbsADT
-- g = mapM_ (mdlWrite WriteFlags {srcDir="/Users/titto/workspace/zm-language/ts",overwrite=False}) $ generate Flags {namespace=["ADT"],primTypes=[],addIndex=True} (absEnv p0)
