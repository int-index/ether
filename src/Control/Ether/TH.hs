{-# LANGUAGE TemplateHaskell #-}
module Control.Ether.TH
    ( ethereal
    , EtherealConfig (..)
    , defaultEtherealReaderConfig
    ) where

import qualified Language.Haskell.TH as TH

import Data.Proxy
import Data.Functor.Identity
import Control.Ether.Core
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

data EtherealConfig
    = EtherealReaderConfig TH.Name String String String String String

defaultEtherealReaderConfig :: TH.Name -> String -> EtherealConfig
defaultEtherealReaderConfig tyname name = EtherealReaderConfig
    tyname
    ("Monad" ++ name)
    ("Tag" ++ name)
    ("run" ++ name ++ "T")
    ("run" ++ name)
    ("ask" ++ name)

ethereal :: EtherealConfig -> TH.DecsQ

ethereal (EtherealReaderConfig
 tyname strEffName
 strTagName strRunTransName
 strRunName strAskName) = do
    let effName = TH.mkName strEffName
        tagName = TH.mkName strTagName
        tag = TH.conT tagName
        runTransName = TH.mkName strRunTransName
        runName = TH.mkName strRunName
        askName = TH.mkName strAskName
    effDecl <- TH.tySynD effName [] [t| MonadEtherReader $tag |]
    tagDecl <- emptyDataDecl tagName
    [tagInst] <- [d| type instance EtherData $tag = $(TH.conT tyname) |]
    runTransFunSig <- do
        (mBndr, m) <- tyVar "m"
        (aBndr, a) <- tyVar "a"
        TH.sigD runTransName
          $ TH.forallT [mBndr, aBndr] (return [])
             [t| EtherReaderT $tag $m $a -> EtherData $tag -> $m $a |]
    runTransFunBody <- funSimple runTransName
        [e| runEtherReaderT (Proxy :: Proxy $tag) |]
    runFunSig <- do
        (aBndr, a) <- tyVar "a"
        TH.sigD runName
          $ TH.forallT [aBndr] (return [])
             [t| EtherReaderT $tag Identity $a -> EtherData $tag -> $a |]
    runFunBody <- funSimple runName
        [e| ((runIdentity.).) $(TH.varE runTransName) |]
    askFunSig <- do
        (mBndr, m) <- tyVar "m"
        TH.sigD askName
          $ TH.forallT [mBndr]
             (sequence [ [t| MonadEtherReader $tag $m |] ])
             [t| $m (EtherData $tag) |]
    askFunBody <-
        let body = [e| etherAsk (Proxy :: Proxy $tag) |]
        in TH.funD askName [ TH.clause [] (TH.normalB body) [] ]
    return [ effDecl, tagDecl, tagInst
           , runTransFunSig, runTransFunBody
           , runFunSig, runFunBody
           , askFunSig, askFunBody ]
