{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
module Main where

import Control.Ether.TH
import Control.Monad.Ether.Reader

import Data.Proxy
import Data.Functor.Identity

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function

etherealReader "AmountTag" "runAmountT" "askAmount" ''Int
type MonadAmount = MonadEtherReader AmountTag

etherealReader "CountTag" "runCountT" "askCount" ''Integer
type MonadCount = MonadEtherReader CountTag

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
    :: Fun (Int, Integer) Integer -> Fun Integer Integer -> Int -> Integer -> Property

layeredLocalLeft k f a1 a2 = property (direct == indirect)
  where
    run = runIdentity . flip runAmountT a1 . flip runCountT a2
    direct = apply k (fromIntegral a1, apply f a2)
    indirect = run $ layeredLocalCore (apply f) (\n m -> apply k (fromIntegral n, m))

layeredLocalRight k f a1 a2 = property (direct == indirect)
  where
    run = runIdentity . flip runCountT a2 . flip runAmountT a1
    direct = apply k (fromIntegral a1, apply f a2)
    indirect = run $ layeredLocalCore (apply f) (\n m -> apply k (fromIntegral n, m))

layeredLocalCore
    :: (MonadAmount m, MonadCount m)
    => (Integer -> Integer) -> (Int -> Integer -> a) -> m a
layeredLocalCore f g = do
    n <- askAmount
    m <- etherLocal (Proxy :: Proxy CountTag) f askCount
    return (g n m)

-- this should not compile
z :: IO Int
z = runAmountT (runAmountT (runAmountT askAmount 20) 10) 5
