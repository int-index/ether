module Regression.T12 (test12) where

import Ether
import Control.Lens

import Test.Tasty
import Test.Tasty.QuickCheck

data Foo

succState :: Enum a => MonadState Foo a m => m ()
succState = modify @Foo succ

testEther :: (Enum a, MonadState Foo (a, a) m) => m (a, a)
testEther = do
  Ether.zoom @Foo _1 succState
  Ether.zoom @Foo _2 $ modify @Foo pred
  get @Foo

model :: Enum a => (a, a) -> (a, a)
model (a1, a2) = (succ a1, pred a2)

runner = evalState @Foo

test12 :: TestTree
test12 = testGroup "T12: State zooming"
  [ testProperty "runner works"
    $ \(a1 :: Integer, a2 :: Integer) -> property
    $ runner testEther (a1, a2) == model (a1, a2)
  ]
