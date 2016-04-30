module Regression.T8 (test8) where

import Control.Ether.Abbr
import Control.Monad.Ether

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
  :: Ether '[Foo <-> Int, Bar <-> Bool] m
  => m String
testEther = do
  tagAttach [tag|Foo|] testMTL1
  tagAttach [tag|Bar|] testMTL2
  a <- gets [tag|Foo|] show
  b <- gets [tag|Bar|] show
  return (a ++ b)

model :: Int -> Bool -> String
model a b = show (negate a) ++ show (not b)

runner1 a b
  = flip (evalState  [tag|Foo|]) a
  . flip (evalStateT [tag|Bar|]) b

runner2 a b
  = flip (evalState  [tag|Bar|]) b
  . flip (evalStateT [tag|Foo|]) a

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
