{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Proxy

import Control.Ether.TH
import Control.Ether.Wrapped
import Control.Monad.Ether.Reader
import Control.Monad.Reader

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function

ethereal (defaultEtherealReaderConfig "Reader1")
ethereal (defaultEtherealReaderConfig "Reader2")

r1 :: Proxy TagReader1
r1 = Proxy

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
wrapCore = ethered r1 ask
