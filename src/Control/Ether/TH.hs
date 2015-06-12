{-# LANGUAGE TemplateHaskell #-}
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

fmapN :: Int -> TH.ExpQ
fmapN 0 = [|id|]
fmapN n = TH.infixApp [|fmap|] [|(.)|] (fmapN (n - 1))

deepN :: Int -> TH.ExpQ -> TH.ExpQ
deepN 0 _ = [|id|]
deepN n f = [| $(fmapN n) $f $(deepN (n-1) f) |]
