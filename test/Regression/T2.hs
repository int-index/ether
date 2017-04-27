module Regression.T2 (test2) where

import Ether

import Test.Tasty
import Test.Tasty.QuickCheck

testEther :: (MonadReader' Integer m, MonadReader' Bool m) => m String
testEther = local' (succ :: Integer -> Integer) $ do
  n :: Integer <- asks' (*2)
  b <- local' not ask'
  return (if b then "" else show n)

runner1 (n :: Integer) = flip runReader' n    . flip runReaderT' True
runner2 (n :: Integer) = flip runReader' True . flip runReaderT' n

test2 :: TestTree
test2 = testGroup "T2: Implicit tags"
  [ testProperty "runner₁ works"
    $ \n -> property
    $ runner1 n testEther == show (succ n * 2)
  , testProperty "runner₂ works"
    $ \n -> property
    $ runner2 n testEther == show (succ n * 2)
  , testProperty "runner₁ == runner₂"
    $ \n -> property
    $ runner1 n testEther == runner2 n testEther
  ]
