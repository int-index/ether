module Regression.T8 (test8) where

import Ether

import qualified Control.Monad.State as T

import Test.Tasty
import Test.Tasty.QuickCheck

data Foo
data Bar

testMTL1 :: T.MonadState Int m => m ()
testMTL1 = T.modify negate

testMTL2 :: T.MonadState Bool m => m ()
testMTL2 = T.modify not

testEther
  :: (MonadState Foo Int m, MonadState Bar Bool m)
  => m String
testEther = do
  tagAttach @Foo testMTL1
  tagAttach @Bar testMTL2
  a <- gets @Foo show
  b <- gets @Bar show
  return (a ++ b)

model :: Int -> Bool -> String
model a b = show (negate a) ++ show (not b)

runner1 a b
  = flip (evalState  @Foo) a
  . flip (evalStateT @Bar) b

runner2 a b
  = flip (evalState  @Bar) b
  . flip (evalStateT @Foo) a

test8 :: TestTree
test8 = testGroup "T8: Multiple tag attachements"
  [ testProperty "runner₁ works"
    $ \a b -> property
    $ runner1 a b testEther == model a b
  , testProperty "runner₂ works"
    $ \a b -> property
    $ runner2 a b testEther == model a b
  , testProperty "runner₁ == runner₂"
    $ \a b -> property
    $ runner1 a b testEther == runner2 a b testEther
  ]
