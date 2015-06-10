{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Ether.Core
import Control.Ether.TH
import Control.Ether.Wrapped
import Control.Monad.Ether.Reader
import Control.Monad.Ether.State
import qualified Control.Monad.Reader as Tagless

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function

ethereal "R1" "r1"
ethereal "R2" "r2"
ethereal "S1" "s1"

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Ether"
    [ testGroup "ReaderT"
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
    run = flip (runReader r1) a1 . flip (runReaderT r2) a2
    (direct, indirect) = layeredLocalCore' k f a1 a2

layeredLocalRight k f a1 a2 = property (direct == run indirect)
  where
    run = flip (runReader r2) a2 . flip (runReaderT r1) a1
    (direct, indirect) = layeredLocalCore' k f a1 a2

layeredLocalCore
    :: (MonadReader R1 r1 m, MonadReader R2 r2 m)
    => (r2 -> r2) -> (r1 -> r2 -> a) -> m a
layeredLocalCore f g = do
    n <- ask r1
    m <- local r2 f (ask r2)
    return (g n m)

layeredLocalCore'
    :: (MonadReader R1 Int m, MonadReader R2 Integer m)
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

wrapCore :: MonadReader R1 Int m => m Int
wrapCore = ethered r1 Tagless.ask


-- Should not compile with `ensureUniqueTags`
uniqueTagsCore :: IO ()
uniqueTagsCore = flip (runReaderT r1) (1 :: Int)
               . flip (runReaderT r1) (True :: Bool)
               . flip (runReaderT r1) (2 :: Int)
               . flip (runReaderT r1) (3 :: Integer)
            {- . ensureUniqueTags -}
               $ do
                    a :: Integer <- ask r1
                    b :: Int <- ask r1
                    c :: Bool <- ask r1
                    Tagless.liftIO $ do
                        print a
                        print b
                        print c

stateCore :: ( MonadState  S1 Int m
             , MonadReader R1 Int m
             , UniqueTags m ) => m ()
stateCore = do
    a <- ask r1
    n <- get s1
    put s1 (n * a)
