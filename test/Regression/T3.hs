module Regression.T3 (test3) where

import Control.Ether.Abbr
import Control.Monad.Ether

import qualified Control.Monad.Reader as T
import qualified Control.Monad.State as T

import Test.Tasty
import Test.Tasty.QuickCheck

ethereal "RTag" "rTag"
ethereal "STag" "sTag"

testMTL :: (T.MonadReader Int m, T.MonadState Int m) => m Int
testMTL = do
  b <- T.get
  a <- T.ask
  T.put (a + b)
  return (a * b)

testEther
  :: Ether '[STag --> Int, STag <-> Int, RTag --> Int] m
  => m (Int, Int, Int)
testEther = local rTag (*2) $ do
  a_mul_b <- tagAttach sTag testMTL
  a_add_b <- get sTag
  modify sTag negate
  c <- ask rTag
  return (a_mul_b, a_add_b, c)

runner1 s r
  = flip (runReader  rTag) (negate r)
  . flip (runReaderT sTag) r
  . flip (runStateT  sTag) s
runner2 s r
  = flip (runReader  rTag) (negate r)
  . flip (runStateT  sTag) s
  . flip (runReaderT sTag) r

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
