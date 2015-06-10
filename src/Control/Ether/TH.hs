{-# LANGUAGE TemplateHaskell #-}
module Control.Ether.TH
    ( ethereal
    , EtherealConfig (..)
    , defaultEtherealReaderConfig
    ) where

import qualified Language.Haskell.TH as TH

import Data.Proxy
import Data.Functor.Identity
import Control.Monad.Ether.Reader

emptyDataDecl :: TH.Name -> TH.DecQ
emptyDataDecl name = TH.dataD (return []) name [] [] []

funSimple :: TH.Name -> TH.ExpQ -> TH.DecQ
funSimple name body = TH.funD name [ TH.clause [] (TH.normalB body) [] ]

tyVar :: String -> TH.Q (TH.TyVarBndr, TH.TypeQ)
tyVar name = do
    thName <- TH.newName name
    let thType = TH.varT thName
    return (TH.PlainTV thName, thType)

proxySimple :: TH.Name -> TH.TypeQ -> TH.Q (TH.Dec, TH.Dec)
proxySimple name ty = do
    sig <- TH.sigD name [t| Proxy $ty |]
    val <- funSimple name [e| Proxy |]
    return (sig, val)

data EtherealConfig
    = EtherealReaderConfig String String String String String String String

defaultEtherealReaderConfig :: String -> EtherealConfig
defaultEtherealReaderConfig name = EtherealReaderConfig
    ("Monad" ++ name)
    ("Tag" ++ name)
    ("tag" ++ name)
    ("run" ++ name ++ "T")
    ("run" ++ name)
    ("ask" ++ name)
    ("local" ++ name)

ethereal :: EtherealConfig -> TH.DecsQ

ethereal (EtherealReaderConfig
 strEffName strTagName strTagProxyName
 strRunTransName strRunName strAskName
 strLocalName) = do
    let effName = TH.mkName strEffName
        tagName = TH.mkName strTagName
        tag = TH.conT tagName
        tagProxyName = TH.mkName strTagProxyName
        runTransName = TH.mkName strRunTransName
        runName = TH.mkName strRunName
        askName = TH.mkName strAskName
        localName = TH.mkName strLocalName
    effDecl <- TH.tySynD effName [] [t| MonadEtherReader $tag |]
    tagDecl <- emptyDataDecl tagName
    (tagProxySig, tagProxyVal) <- proxySimple tagProxyName tag
    runTransFunSig <- do
        (mBndr, m) <- tyVar "m"
        (aBndr, a) <- tyVar "a"
        (rBndr, r) <- tyVar "r"
        TH.sigD runTransName
          $ TH.forallT [mBndr, aBndr, rBndr] (return [])
             [t| EtherReaderT $tag $r $m $a -> $r -> $m $a |]
    runTransFunBody <- funSimple runTransName
        [e| runEtherReaderT (Proxy :: Proxy $tag) |]
    runFunSig <- do
        (aBndr, a) <- tyVar "a"
        (rBndr, r) <- tyVar "r"
        TH.sigD runName
          $ TH.forallT [aBndr, rBndr] (return [])
             [t| EtherReaderT $tag $r Identity $a -> $r -> $a |]
    runFunBody <- funSimple runName
        [e| ((runIdentity.).) $(TH.varE runTransName) |]
    askFunSig <- do
        (mBndr, m) <- tyVar "m"
        (rBndr, r) <- tyVar "r"
        TH.sigD askName
          $ TH.forallT [mBndr, rBndr]
             (sequence [ [t| MonadEtherReader $tag $r $m |] ])
             [t| $m $r |]
    askFunBody <- funSimple askName [e| etherAsk (Proxy :: Proxy $tag) |]
    localFunSig <- do
        (mBndr, m) <- tyVar "m"
        (aBndr, a) <- tyVar "a"
        (rBndr, r) <- tyVar "r"
        TH.sigD localName
          $ TH.forallT [mBndr, aBndr, rBndr]
             (sequence [ [t| MonadEtherReader $tag $r $m |] ])
             [t| ($r -> $r) -> $m $a -> $m $a |]
    localFunBody <- funSimple localName [e| etherLocal (Proxy :: Proxy $tag) |]
    return [ effDecl, tagDecl
           , tagProxySig, tagProxyVal
           , runTransFunSig, runTransFunBody
           , runFunSig, runFunBody
           , askFunSig, askFunBody
           , localFunSig, localFunBody ]
