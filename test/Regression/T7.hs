module Regression.T7 (test7) where

import Data.Monoid
import Control.Monad

import Ether
import qualified Control.Monad.Writer as T

import Test.Tasty
import Test.Tasty.QuickCheck

data WTag

testEther
  :: Num a
  => T.MonadWriter (Sum a) m
  => MonadWriter WTag (Sum a) m
  => [a] -> m ()
testEther xs = do
  forM_ xs $ \x -> do
    u1 <- T.tell (Sum x)
    u2 <- tell @WTag (Sum 1)
    when (u1 /= u2) $ error "Impossible"

runner1 :: Num a => [a] -> (a, a)
runner1 xs =
  let (s, c) = T.runWriter . execWriterT @WTag $ testEther xs
  in (getSum s, getSum c)

runner2 :: Num a => [a] -> (a, a)
runner2 xs =
  let (c, s) = runWriter @WTag . T.execWriterT $ testEther xs
  in (getSum s, getSum c)

triangular :: Integral a => a -> a
triangular n = div (n * (n + 1)) 2

test7 :: TestTree
test7 = testGroup "T7: Triangular via Ether"
  [ testProperty "runner₁ works"
    $ \n -> property
    $ let n' = abs n :: Integer
      in runner1 [1..n'] == (n', triangular n')
  , testProperty "runner₂ works"
    $ \n -> property
    $ let n' = abs n :: Integer
      in runner2 [1..n'] == (n', triangular n')
  , testProperty "runner₁ == runner₂"
    $ \(ns :: [Integer]) -> property
    $ runner1 ns == runner2 ns
  ]
