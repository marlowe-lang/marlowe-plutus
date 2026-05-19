module Language.Marlowe.Runtime.Web.Adapter.Control.Lens where

import Control.Lens (
  DefName (TopName),
  lensField,
  lensRules,
  makeLensesWith,
  (&),
  (.~),
 )
import Language.Haskell.TH (DecsQ, Name, mkName, nameBase)

makeSuffixedLenses :: Name -> DecsQ
makeSuffixedLenses =
  makeLensesWith $
    lensRules
      & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name ++ "L")]
