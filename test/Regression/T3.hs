module Regression.T3 (test3) where

import Ether

import qualified Control.Monad.Reader as T
import qualified Control.Monad.State as T

import Test.Tasty
import Test.Tasty.QuickCheck

data RTag
data STag

testMTL :: (T.MonadReader Int m, T.MonadState Int m) => m Int
testMTL = do
  b <- T.get
  a <- T.ask
  T.put (a + b)
  return (a * b)

testEther
  :: (MonadReader STag Int m, MonadState STag Int m, MonadReader RTag Int m)
  => m (Int, Int, Int)
testEther = local @RTag (*2) $ do
  a_mul_b <- tagAttach @STag testMTL
  a_add_b <- get @STag
  modify @STag negate
  c <- ask @RTag
  return (a_mul_b, a_add_b, c)

runner1 s r
  = flip (runReader  @RTag) (negate r)
  . flip (runReaderT @STag) r
  . flip (runStateT  @STag) s
runner2 s r
  = flip (runReader  @RTag) (negate r)
  . flip (runStateT  @STag) s
  . flip (runReaderT @STag) r

test3 :: TestTree
test3 = testGroup "T3: Tag attachement"
  [ testProperty "runner₁ works"
    $ \s r -> property
    $ (==)
        (runner1 s r testEther)
        ((s * r, s + r, negate r * 2), negate (s + r))
  , testProperty "runner₂ works"
    $ \s r -> property
    $ (==)
        (runner2 s r testEther)
        ((s * r, s + r, negate r * 2), negate (s + r))
  , testProperty "runner₁ == runner₂"
    $ \s r -> property
    $ runner1 s r testEther == runner2 s r testEther
  ]
