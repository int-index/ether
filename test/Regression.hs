{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Main where

import Control.Ether.TH
import Control.Monad.Ether.Reader

import Data.Proxy

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function

ethereal (defaultEtherealReaderConfig ''Int "Amount")
ethereal (defaultEtherealReaderConfig ''Integer "Count")

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
    run = flip runAmount a1 . flip runCountT a2
    (direct, indirect) = layeredLocalCore' k f a1 a2

layeredLocalRight k f a1 a2 = property (direct == run indirect)
  where
    run = flip runCount a2 . flip runAmountT a1
    (direct, indirect) = layeredLocalCore' k f a1 a2

layeredLocalCore
    :: (MonadAmount m, MonadCount m)
    => (Integer -> Integer) -> (Int -> Integer -> a) -> m a
layeredLocalCore f g = do
    n <- askAmount
    m <- etherLocal (Proxy :: Proxy TagCount) f askCount
    return (g n m)

layeredLocalCore'
    :: (MonadAmount m, MonadCount m)
    => Fun (Int, Integer) Integer
    -> Fun Integer Integer
    -> Int -> Integer -> (Integer, m Integer)
layeredLocalCore' k f a1 a2 = (direct, indirect)
  where
    direct = apply k (fromIntegral a1, apply f a2)
    indirect = layeredLocalCore (apply f) (\n m -> apply k (fromIntegral n, m))
