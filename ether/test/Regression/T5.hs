module Regression.T5 (test5) where

import Control.Ether.Abbr

import qualified Control.Monad.Ether.Implicit as I
import qualified Control.Ether.Implicit.Abbr as I

import Test.Tasty
import Test.Tasty.QuickCheck

newtype Counter = Counter Int
  deriving (Eq, Ord, Num, Enum)

incCounter :: Counter -> Counter
incCounter = succ

testEther
  :: (Num a, Ord a)
  => Ether '[I.R a, I.S Counter] m
  => m a
testEther = do
  a <- I.ask
  if a <= 0
    then do
      I.put (0 :: Counter)
      return 1
    else do
      I.modify incCounter -- overriden in the base case
      b <- I.runReaderT testEther (a - 1)
      I.modify incCounter
      return (a * b)

runner :: (Num a, Ord a) => a -> (a, Counter)
runner a = I.runState (I.runReaderT testEther a) (0 :: Counter)

factorial :: (Num a, Enum a) => a -> a
factorial a = product [1..a]

test5 :: TestTree
test5 = testGroup "T5: Factorial via Ether"
  [ testProperty "runner works"
    $ \n -> property
    $ let n' = fromIntegral n :: Integer
      in runner n' == (factorial n', max 0 (Counter n))
  ]
