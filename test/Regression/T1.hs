module Regression.T1 (test1) where

import Ether
import TupleInstances ()
import Data.List (group)

import Test.Tasty
import Test.Tasty.QuickCheck

data Tag1
data Tag2

testEther
  :: (MonadReader Tag1 String m, MonadReader Tag2 String m)
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
runner3 s1 s2 = flip runReaders (Tagged @Tag1 s1, Tagged @Tag2 s2)

same :: Eq a => [a] -> Bool
same = (<=1) . length . group

test1 :: TestTree
test1 = testGroup "T1: Reader local environment"
  [ testProperty "runner₁ works"
      $ \s1 s2 -> property
      $ runner1 s1 s2 testEther == ((s1, s2), (s1, map succ s2))
  , testProperty "runner₂ works"
      $ \s1 s2 -> property
      $ runner2 s1 s2 testEther == ((s1, s2), (s1, map succ s2))
  , testProperty "runner₃ works"
      $ \s1 s2 -> property
      $ runner3 s1 s2 testEther == ((s1, s2), (s1, map succ s2))
  , testProperty "runner₁ == runner₂ == runner₃"
      $ \s1 s2 -> property
      $ same
        [ runner1 s1 s2 testEther
        , runner2 s1 s2 testEther
        , runner3 s1 s2 testEther ]
  ]
