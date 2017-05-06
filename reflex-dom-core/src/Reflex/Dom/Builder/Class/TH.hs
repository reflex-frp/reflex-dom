module Reflex.Dom.Builder.Class.TH where

import Control.Lens
import Language.Haskell.TH.Lib (DecsQ)
import Language.Haskell.TH.Syntax (Name, nameBase)

namer :: [String] -> Name -> [Name] -> Name -> [DefName]
namer s n ks t | nameBase t `elem` s = []
               | otherwise = underscoreNoPrefixNamer n ks t

makeLensesWithoutField :: [String] -> Name -> DecsQ
makeLensesWithoutField s = makeLensesWith (lensRules & lensField .~ namer s)
