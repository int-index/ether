module Regression.T1 (test1) where

import Control.Monad.Ether
import Control.Ether.Abbr

import Test.Tasty
import Test.Tasty.QuickCheck

data Tag1
data Tag2

testEther
  :: Ether '[Tag1 --> String, Tag2 --> String] m
  => m ((String, String), (String, String))
testEther = do
  s1 <- ask @Tag1
  s2 <- ask @Tag2
  let s1s2 = (s1, s2)
  s1s2' <- local @Tag2 (map succ) $ do
    s1' <- ask @Tag1
    s2' <- ask @Tag2
    return (s1', s2')
  return (s1s2, s1s2')

runner1 s1 s2 = flip (runReader @Tag1) s1 . flip (runReaderT @Tag2) s2
runner2 s1 s2 = flip (runReader @Tag2) s2 . flip (runReaderT @Tag1) s1

test1 :: TestTree
test1 = testGroup "T1: Reader local environment"
  [ testProperty "runner₁ works"
      $ \s1 s2 -> property
      $ runner1 s1 s2 testEther == ((s1, s2), (s1, map succ s2))
  , testProperty "runner₂ works"
      $ \s1 s2 -> property
      $ runner2 s1 s2 testEther == ((s1, s2), (s1, map succ s2))
  , testProperty "runner₁ == runner₂"
      $ \s1 s2 -> property
      $ runner1 s1 s2 testEther == runner2 s1 s2 testEther
  ]
