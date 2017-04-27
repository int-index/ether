module Regression.T4 (test4) where

import Ether

import Test.Tasty
import Test.Tasty.QuickCheck

data RTag

testEther :: MonadReader RTag Int m => m Int
testEther = ask @RTag

runner r
  = flip (runReader  @RTag) (r' :: Int)
  . flip (runReaderT @RTag) (r  :: Int)
  where
    r' = negate r

test4 :: TestTree
test4 = testGroup "T4: Nested same-tag readers"
  [ testProperty "runner works"
    $ \r -> property
    $ runner r testEther == r
  ]
