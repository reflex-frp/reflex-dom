module Reflex.Dom.Builder.Class.TH where

import Control.Lens
import Language.Haskell.TH.Syntax (Name, nameBase)
import Language.Haskell.TH.Lib (DecsQ)

namer :: String -> Name -> [Name] -> Name -> [DefName]
namer s n ks t | s == nameBase t = []
               | otherwise = underscoreNoPrefixNamer n ks t

makeLensesWithoutField :: String -> Name -> DecsQ
makeLensesWithoutField s = makeLensesWith (lensRules & lensField .~ namer s)
