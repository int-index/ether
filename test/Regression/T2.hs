module Regression.T2 (test2) where

import Control.Ether.Abbr
import qualified Control.Monad.Ether.Implicit as I
import qualified Control.Ether.Implicit.Abbr as I

import Test.Tasty
import Test.Tasty.QuickCheck

testEther :: Ether '[I.R Integer, I.R Bool] m => m String
testEther = I.local (succ :: Integer -> Integer) $ do
  n :: Integer <- I.ask
  b <- I.local not I.ask
  return (if b then "" else show n)

runner1 (n :: Integer) = flip I.runReader n . flip I.runReaderT True
runner2 (n :: Integer) = flip I.runReader True . flip I.runReaderT n

test2 :: TestTree
test2 = testGroup "T2: Implicit tags"
  [ testProperty "runner₁ works"
    $ \n -> property
    $ runner1 n testEther == show (succ n)
  , testProperty "runner₂ works"
    $ \n -> property
    $ runner2 n testEther == show (succ n)
  , testProperty "runner₁ == runner₂"
    $ \n -> property
    $ runner1 n testEther == runner2 n testEther
  ]
