module Ether.Internal.TH_Utils
  ( tupCount
  , varName
  , tagName
  ) where

import qualified Language.Haskell.TH as TH

tupCount :: Int
tupCount = 62

varName, tagName :: Int -> TH.Name
varName k = TH.mkName ('a' : show k)
tagName k = TH.mkName ('t' : show k)
