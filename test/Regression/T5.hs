module Regression.T5 (test5) where

import Ether

import Test.Tasty
import Test.Tasty.QuickCheck

newtype Counter = Counter Int
  deriving (Eq, Ord, Num, Enum)

incCounter :: Counter -> Counter
incCounter = succ

testEther
  :: (Num a, Ord a)
  => (MonadReader' a m, MonadState' Counter m)
  => m a
testEther = do
  a <- ask'
  if a <= 0
    then do
      put' (0 :: Counter)
      return 1
    else do
      modify' incCounter -- overriden in the base case
      b <- runReaderT' testEther (a - 1)
      modify' incCounter
      return (a * b)

runner :: (Num a, Ord a) => a -> (a, Counter)
runner a = runState' (runReaderT' testEther a) (0 :: Counter)

factorial :: (Num a, Enum a) => a -> a
factorial a = product [1..a]

test5 :: TestTree
test5 = testGroup "T5: Factorial via Ether"
  [ testProperty "runner works"
    $ \n -> property
    $ let n' = fromIntegral n :: Integer
      in runner n' == (factorial n', max 0 (Counter n))
  ]
