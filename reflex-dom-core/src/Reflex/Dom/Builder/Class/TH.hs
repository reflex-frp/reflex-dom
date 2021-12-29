module Reflex.Dom.Builder.Class.TH where

import Lens.Micro.GHC
import Lens.Micro.TH
import Language.Haskell.TH ( mkName, Name, DecsQ, nameBase )
import Data.Char (toLower)

namer :: [String] -> Name -> [Name] -> Name -> [DefName]
namer s n ks t | nameBase t `elem` s = []
               | otherwise = underscoreNoPrefixNamer n ks t

makeLensesWithoutField :: [String] -> Name -> DecsQ
makeLensesWithoutField s = makeLensesWith (lensRules & lensField .~ namer s)

-- copied from lens
underscoreNoPrefixNamer :: Name -> [Name] -> Name -> [DefName]
underscoreNoPrefixNamer _ _ n =
  case nameBase n of
    '_':x:xs -> [TopName (mkName (toLower x:xs))]
    _        -> []
