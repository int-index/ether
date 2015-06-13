{-# LANGUAGE TemplateHaskell #-}

-- | Template haskell utilities.

module Control.Ether.TH
    ( ethereal
    , fmapN
    , deepN
    ) where

import qualified Language.Haskell.TH as TH

import Data.Proxy

emptyDataDecl :: TH.Name -> TH.DecQ
emptyDataDecl name = TH.dataD (return []) name [] [] []

funSimple :: TH.Name -> TH.ExpQ -> TH.DecQ
funSimple name body = TH.funD name [ TH.clause [] (TH.normalB body) [] ]

proxySimple :: TH.Name -> TH.TypeQ -> TH.Q (TH.Dec, TH.Dec)
proxySimple name ty = do
    sig <- TH.sigD name [t| Proxy $ty |]
    val <- funSimple name [e| Proxy |]
    return (sig, val)

-- |
-- Creates a tag and a value-level proxy for it.
--
-- @'ethereal' \"Foo\" \"foo\"@ generates the following code:
-- 
-- > data Foo
-- > foo :: Proxy Foo
-- > foo = Proxy
ethereal :: String -> String -> TH.DecsQ
ethereal strTagName strTagProxyName = do
    let tagName = TH.mkName strTagName
        tag = TH.conT tagName
        tagProxyName = TH.mkName strTagProxyName
    tagDecl <- emptyDataDecl tagName
    (tagProxySig, tagProxyVal) <- proxySimple tagProxyName tag
    return [tagDecl, tagProxySig, tagProxyVal]

-- |
-- Compose 'fmap' @n@ times.
--
-- @$('fmapN' 5) = fmap.fmap.fmap.fmap.fmap@
fmapN :: Int -> TH.ExpQ
fmapN 0 = [|id|]
fmapN n = TH.infixApp [|fmap|] [|(.)|] (fmapN (n - 1))

-- |
-- 'fmap' a function @n@ levels deep.
--
-- @$('deepN' 3 [| f |]) = $('fmapN' 3) f ($('fmapN' 2) f ($('fmapN' 1) f id))@
deepN :: Int -> TH.ExpQ -> TH.ExpQ
deepN 0 _ = [|id|]
deepN n f = [| $(fmapN n) $f $(deepN (n-1) f) |]
