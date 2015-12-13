module Main where

import Data.Monoid
import Control.Monad

import Control.Monad.Ether
import Control.Ether.Abbr

import qualified Control.Monad.Ether.Implicit as I
import qualified Control.Ether.Implicit.Abbr as I

import qualified Control.Monad.Reader as T
import qualified Control.Monad.Writer as T
import qualified Control.Monad.State  as T

import Test.Tasty

import Regression.T1
import Regression.T2
import Regression.T3

ethereal "R1" "r1"
ethereal "R2" "r2"
ethereal "S1" "s1"
ethereal "Foo" "foo"
ethereal "Bar" "bar"

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Ether"
    [ test1
    , test2
    , test3
    ]

wrapCore :: (T.MonadReader Int m, T.MonadState Int m) => m Int
wrapCore = do
    b <- T.get
    a <- T.ask
    T.put (a + b)
    return (a * b)

wrapCore' :: Ether '[S1 --> Int, S1 <-> Int, R1 --> Int] m => m Int
wrapCore' = do
    a <- tagAttach s1 wrapCore
    c <- ask r1
    return (a + c)

wrapCore'' :: Int -> (Int, Int)
wrapCore'' a = runReader r1 (runStateT s1 (runReaderT s1 wrapCore' a) a) (-1)

nonUniqueTagsCore :: IO ()
nonUniqueTagsCore
  = flip (runReaderT r1) (1 :: Int)
  . flip (runReaderT r1) (True :: Bool)
  . flip (runReaderT r1) (2 :: Int)
  . flip (runReaderT r1) (3 :: Integer)
  $ do
    a :: Integer <- ask r1
    b :: Int <- ask r1
    c :: Bool <- ask r1
    T.liftIO $ do
      print a
      print b
      print c

stateCore :: Ether '[S1 <-> Int, R1 --> Int] m => m ()
stateCore = do
    a <- ask r1
    n <- get s1
    put s1 (n * a)
    modify s1 (subtract 1)

recurseCore :: (Num a, Ord a) => Ether '[I.R a, I.S Int]m => m a
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
       , Ether '[I.E DivideByZero, I.E (NegativeLog a)] m
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

wrapState_useboth :: Ether '[Foo <-> Int, Bar <-> Bool] m => m String
wrapState_useboth = do
    a <- tagAttach foo wrapState_f
    b <- tagAttach bar wrapState_g
    return (a ++ b)

wrapStateCore :: Int -> Bool -> String
wrapStateCore int bool = evalState foo (evalStateT bar wrapState_useboth bool) int

wrapStateBad1_g :: MonadState Foo Int m => m ()
wrapStateBad1_g = modify foo (*100)

wrapStateBad1_useboth :: MonadState Foo Int m => m String
wrapStateBad1_useboth = do
    wrapStateBad1_g
    tagAttach foo wrapState_f

wrapStateBad1 :: Int -> String
wrapStateBad1 = evalState foo wrapStateBad1_useboth

wrapStateBad2 :: (T.MonadState Int m , MonadState Foo Int m) => m Int
wrapStateBad2 = do
    modify foo (*100)
    T.get

wrapStateBad2Core :: Int -> Int
wrapStateBad2Core = evalState foo (tagAttach foo wrapStateBad2)

wrapReaderCore :: Int -> Int
wrapReaderCore = runReader foo (tagAttach foo (liftM2 (+) T.ask (ask foo)))
