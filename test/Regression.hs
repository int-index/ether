{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Monoid
import Control.Monad
import Control.Ether.Tagged
import Control.Ether.TH
import Control.Ether.Wrapped
import Control.Monad.Ether.Reader
import Control.Monad.Ether.State
import Control.Monad.Ether.Writer
import qualified Control.Monad.Ether.Implicit.Reader as I
import qualified Control.Monad.Ether.Implicit.State  as I
import qualified Control.Monad.Ether.Implicit.Except as I
import qualified Control.Monad.Reader as T
import qualified Control.Monad.Writer as T
import qualified Control.Monad.State  as T

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Function

ethereal "R1" "r1"
ethereal "R2" "r2"
ethereal "S1" "s1"
ethereal "Foo" "foo"
ethereal "Bar" "bar"

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

implicitCore :: (I.MonadReader Int m, I.MonadReader Bool m) => m String
implicitCore = I.local (succ :: Int -> Int) $ do
    n :: Int <- I.ask
    b <- I.local not I.ask
    return (if b then "" else show n)

wrapCore
    :: ( T.MonadReader Int m
       , T.MonadState  Int m
       ) => m Int
wrapCore = do
    b <- T.get
    a <- T.ask
    T.put (a + b)
    return (a * b)

wrapCore'
    :: ( MonadReader S1 Int m
       , MonadState S1 Int m
       , MonadReader R1 Int m
       ) => m Int
wrapCore' = do
    a <- ethered s1 wrapCore
    c <- ask r1
    return (a + c)

wrapCore'' :: Int -> (Int, Int)
wrapCore'' a = runReader r1 (runStateT s1 (runReaderT s1 wrapCore' a) a) (-1)

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
                    T.liftIO $ do
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
    modify s1 (subtract 1)

recurseCore :: (Num a, Ord a) => (I.MonadReader a m, I.MonadState Int m)  => m a
recurseCore = do
    a <- I.ask
    if (a <= 0)
        then do
            I.put (0 :: Int)
            return 1
        else do
            I.modify (succ :: Int -> Int)
            b <- I.runReaderT recurseCore (a - 1)
            I.modify (succ :: Int -> Int)
            return (a * b)

factorial :: (Num a, Ord a) => a -> (a, Int)
factorial a = I.runState (I.runReaderT recurseCore a) (0 :: Int)

factorial' :: Int -> (Int, Int)
factorial' a = factorial (a :: Int)

data DivideByZero = DivideByZero
    deriving (Show)
data NegativeLog a = NegativeLog a
    deriving (Show)

exceptCore
    :: ( Floating a, Ord a
       , I.MonadExcept DivideByZero m
       , I.MonadExcept (NegativeLog a) m
       ) => a -> a -> m a
exceptCore a b = do
    T.when (b == 0) (I.throw DivideByZero)
    let d = a /b
    T.when (d < 0) (I.throw (NegativeLog d))
    return (log d)

(&) :: a -> (a -> c) -> c
(&) = flip ($)

exceptCore' :: Double -> Double -> String
exceptCore' a b = do
    liftM show (exceptCore a b)
    &I.handleT (\(NegativeLog (x::Double)) -> "nl: " ++ show x)
    &I.handle  (\DivideByZero -> "dz")

summatorCore
    :: ( Num a
       , T.MonadWriter (Sum a) m
       , MonadWriter Foo (Sum a) m
       ) => [a] -> m ()
summatorCore xs = do
    forM_ xs $ \x -> do
        T.tell (Sum x)
        tell foo (Sum 1)

summatorCore' :: Num a => [a] -> (Sum a, Sum a)
summatorCore' = runWriter foo . T.execWriterT . summatorCore

wrapState_f :: T.MonadState Int m => m String
wrapState_f = liftM show T.get

wrapState_g :: T.MonadState Bool m => m String
wrapState_g = liftM show T.get

wrapState_useboth
    :: ( MonadState Foo Int  m
       , MonadState Bar Bool m
       ) => m String
wrapState_useboth = do
    a <- ethered foo wrapState_f
    b <- ethered bar wrapState_g
    return (a ++ b)

wrapStateCore :: Int -> Bool -> String
wrapStateCore int bool = evalState foo (evalStateT bar wrapState_useboth bool) int

wrapStateBad1_g :: MonadState Foo Int m => m ()
wrapStateBad1_g = modify foo (*100)

wrapStateBad1_useboth :: MonadState Foo Int m => m String
wrapStateBad1_useboth = do
    wrapStateBad1_g
    ethered foo wrapState_f

wrapStateBad1 :: Int -> String
wrapStateBad1 = evalState foo wrapStateBad1_useboth

wrapStateBad2
    :: ( T.MonadState Int m
       , MonadState Foo Int m
       ) => m Int
wrapStateBad2 = do
    modify foo (*100)
    T.get

wrapStateBad2Core :: Int -> Int
wrapStateBad2Core = evalState foo (ensureUniqueTags $ ethered foo wrapStateBad2)
