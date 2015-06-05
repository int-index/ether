{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
module Main where

import Control.Monad.Ether.Reader

import Data.Proxy
import Data.Functor.Identity
import Control.Ether.Core

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function

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
    run = runIdentity . flip runAmountT a1 . flip runAmount2T a2
    direct = apply k (fromIntegral a1, apply f a2)
    indirect = run $ layeredLocalCore (apply f) (\n m -> apply k (fromIntegral n, m))

layeredLocalRight k f a1 a2 = property (direct == indirect)
  where
    run = runIdentity . flip runAmount2T a2 . flip runAmountT a1
    direct = apply k (fromIntegral a1, apply f a2)
    indirect = run $ layeredLocalCore (apply f) (\n m -> apply k (fromIntegral n, m))

layeredLocalCore
    :: (MonadEtherReader AmountTag m, MonadEtherReader Amount2Tag m)
    => (Integer -> Integer) -> (Int -> Integer -> a) -> m a
layeredLocalCore f g = do
    n <- askAmount
    m <- etherLocal (Proxy :: Proxy Amount2Tag) f askAmount2
    return (g n m)

data AmountTag
type instance EtherData AmountTag = Int

runAmountT :: EtherReaderT AmountTag m a -> EtherData AmountTag -> m a
runAmountT = runEtherReaderT (Proxy :: Proxy AmountTag)

askAmount :: MonadEtherReader AmountTag m => m (EtherData AmountTag)
askAmount = etherAsk (Proxy :: Proxy AmountTag)

data Amount2Tag
type instance EtherData Amount2Tag = Integer

runAmount2T :: EtherReaderT Amount2Tag m a -> EtherData Amount2Tag -> m a
runAmount2T = runEtherReaderT (Proxy :: Proxy Amount2Tag)

askAmount2 :: MonadEtherReader Amount2Tag m => m (EtherData Amount2Tag)
askAmount2 = etherAsk (Proxy :: Proxy Amount2Tag)

-- this should not compile
z :: IO Int
z = runAmountT (runAmountT (runAmountT askAmount 20) 10) 5
