{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

-- | Template Haskell utilities.

module Control.Ether.TH (tag) where

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

import GHC.Prim (Proxy#, proxy#)

tag :: TH.QuasiQuoter
tag = TH.QuasiQuoter proxyExpQ undefined undefined undefined
  where
    proxyExpQ :: String -> TH.ExpQ
    proxyExpQ nameStr =
      let ty = TH.conT (TH.mkName nameStr)
      in [e| proxy# :: Proxy# $ty |]
