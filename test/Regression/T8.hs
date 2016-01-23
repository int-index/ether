module Regression.T8 (test8) where

import Control.Ether.Abbr
import Control.Monad.Ether

import qualified Control.Monad.State as T

import Test.Tasty
import Test.Tasty.QuickCheck

ethereal "Foo" "foo"
ethereal "Bar" "bar"

testMTL1 :: T.MonadState Int m => m ()
testMTL1 = T.modify negate

testMTL2 :: T.MonadState Bool m => m ()
testMTL2 = T.modify not

testEther
  :: Ether '[Foo <-> Int, Bar <-> Bool] m
  => m String
testEther = do
  tagAttach foo testMTL1
  tagAttach bar testMTL2
  a <- gets foo show
  b <- gets bar show
  return (a ++ b)

model :: Int -> Bool -> String
model a b = show (negate a) ++ show (not b)

runner1 a b
  = flip (evalState  foo) a
  . flip (evalStateT bar) b

runner2 a b
  = flip (evalState  bar) b
  . flip (evalStateT foo) a

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
