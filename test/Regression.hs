{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Proxy

import Control.Ether.Core
import Control.Ether.TH
import Control.Ether.Wrapped
import Control.Monad.Ether.Reader
import Control.Monad.Ether.State
import Control.Monad.Reader

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function

ethereal (defaultEtherealReaderConfig "Reader1")
ethereal (defaultEtherealReaderConfig "Reader2")

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Ether"
    [ testGroup "EtherReaderT"
        [ testProperty "layered sum (local left)" layeredLocalLeft
        , testProperty "layered sum (local right)" layeredLocalRight
        ]
    ]

layeredLocalLeft, layeredLocalRight
    :: Fun (Int, Integer) Integer
    -> Fun Integer Integer
    -> Int -> Integer -> Property

layeredLocalLeft k f a1 a2 = property (direct == run indirect)
  where
    run = flip runReader1 a1 . flip runReader2T a2
    (direct, indirect) = layeredLocalCore' k f a1 a2

layeredLocalRight k f a1 a2 = property (direct == run indirect)
  where
    run = flip runReader2 a2 . flip runReader1T a1
    (direct, indirect) = layeredLocalCore' k f a1 a2

layeredLocalCore
    :: (MonadReader1 r1 m, MonadReader2 r2 m)
    => (r2 -> r2) -> (r1 -> r2 -> a) -> m a
layeredLocalCore f g = do
    n <- askReader1
    m <- localReader2 f askReader2
    return (g n m)

layeredLocalCore'
    :: (MonadReader1 Int m, MonadReader2 Integer m)
    => Fun (Int, Integer) Integer
    -> Fun Integer Integer
    -> Int -> Integer -> (Integer, m Integer)
layeredLocalCore' k f a1 a2 = (direct, indirect)
  where
    direct = apply k (fromIntegral a1, apply f a2)
    indirect = layeredLocalCore (apply f) (\n m -> apply k (fromIntegral n, m))

inferCore :: (MonadReader' Int m, MonadReader' Bool m) => m String
inferCore = local' (succ :: Int -> Int) $ do
    n :: Int <- ask'
    b <- local' not ask'
    return (if b then "" else show n)

wrapCore :: MonadEtherReader TagReader1 Int m => m Int
wrapCore = ethered tagReader1 ask


-- Should not compile with `ensureUniqueEtherTags`
uniqueTagsCore :: IO ()
uniqueTagsCore = flip runReader1T (1 :: Int)
               . flip runReader1T (True :: Bool)
               . flip runReader1T (2 :: Int)
               . flip runReader1T (3 :: Integer)
            {- . ensureUniqueEtherTags -}
               $ do
                    a :: Integer <- askReader1
                    b :: Int <- askReader1
                    c :: Bool <- askReader1
                    liftIO $ do
                        print a
                        print b
                        print c

data TagState1

stateCore :: ( MonadEtherState  TagState1  Int m
             , MonadEtherReader TagReader1 Int m
             , UniqueEtherTags m ) => m ()
stateCore = do
    a <- etherAsk (Proxy :: Proxy TagReader1)
    n <- etherGet (Proxy :: Proxy TagState1)
    etherPut (Proxy :: Proxy TagState1) (n * a)