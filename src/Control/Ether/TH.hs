{-# LANGUAGE TemplateHaskell #-}
module Control.Ether.TH
    ( etherealReader
    ) where

import qualified Language.Haskell.TH as TH

import Data.Proxy
import Control.Ether.Core
import Control.Monad.Ether.Reader

emptyDataDecl :: TH.Name -> TH.DecQ
emptyDataDecl name = TH.dataD (return []) name [] [] []

etherealReader :: String -> String -> String -> TH.Name -> TH.DecsQ
etherealReader strTagName strRunName strAskName tyname = do
    let tagName = TH.mkName strTagName
        tag = TH.conT tagName
        runName = TH.mkName strRunName
        askName = TH.mkName strAskName
    tagDecl <- emptyDataDecl tagName
    [tagInst] <- [d| type instance EtherData $tag = $(TH.conT tyname) |]
    runFunSig <- do
        mName <- TH.newName "m"
        aName <- TH.newName "a"
        let m' = TH.varT mName
            a' = TH.varT aName
            ty = TH.forallT [TH.PlainTV mName, TH.PlainTV aName] (return [])
                [t| EtherReaderT $tag $m' $a' -> EtherData $tag -> $m' $a' |]
        TH.sigD runName ty
    runFunBody <-
        let body = [e| runEtherReaderT (Proxy :: Proxy $tag) |]
        in TH.funD runName [ TH.clause [] (TH.normalB body) [] ]
    askFunSig <- do
        mName <- TH.newName "m"
        let m' = TH.varT mName
            ty = TH.forallT [TH.PlainTV mName]
                (sequence [ [t| MonadEtherReader $tag $m' |] ])
                [t| $m' (EtherData $tag) |]
        TH.sigD askName ty
    askFunBody <-
        let body = [e| etherAsk (Proxy :: Proxy $tag) |]
        in TH.funD askName [ TH.clause [] (TH.normalB body) [] ]
    return [ tagDecl, tagInst, runFunSig, runFunBody, askFunSig, askFunBody ]
