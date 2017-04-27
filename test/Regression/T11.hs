module Regression.T11 (test11) where

import Ether

import Data.Bool
import qualified Control.Monad.State as T
import qualified Control.Monad.Reader as T

import Test.Tasty
import Test.Tasty.QuickCheck

data STag

testEther
  :: ( MonadState STag [Integer] m
     , T.MonadState Bool m
     , T.MonadReader Integer m )
  => m ()
testEther = do
  T.modify not
  f <- bool negate id <$> T.get
  n <- T.ask
  T.local succ testEther
  modify @STag (f n:)

model :: Integer -> [Integer]
model n = zipWith ($) (cycle [id, negate]) [n..]

runner1 n
  = flip (T.runReader) n
  . flip (execLazyStateT @STag) []
  . flip T.evalStateT False

runner2 n
  = flip T.evalState False
  . flip (execLazyStateT @STag) []
  . flip T.runReaderT n

test11 :: TestTree
test11 = testGroup "T11: Lazy sequence"
  [ testProperty "runner₁ works"
    $ \l n -> property
    $ take l (runner1 n testEther) == take l (model n)
  , testProperty "runner₂ works"
    $ \l n -> property
    $ take l (runner2 n testEther) == take l (model n)
  , testProperty "runner₁ == runner₂"
    $ \l n -> property
    $ take l (runner1 n testEther) == take l (runner2 n testEther)
  ]
