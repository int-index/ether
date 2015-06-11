{-# LANGUAGE TemplateHaskell #-}
module Control.Ether.TH (ethereal) where

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
