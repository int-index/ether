module Regression.T6 (test6) where

import Control.Monad
import Control.Ether.Abbr

import qualified Control.Monad.Ether.Implicit as I
import qualified Control.Ether.Implicit.Abbr as I

import Test.Tasty
import Test.Tasty.QuickCheck

data DivideByZero = DivideByZero
    deriving (Show)

data NegativeLog a = NegativeLog a
    deriving (Show)

testEther
  :: (Floating a, Ord a)
  => Ether '[I.E DivideByZero, I.E (NegativeLog a)] m
  => a -> a -> m a
testEther a b = do
  when (b == 0) (I.throw DivideByZero)
  let d = a / b
  when (d < 0) (I.throw (NegativeLog d))
  return (log d)

-- Copied verbatim from "Data.Function" to support @base < 4.8@.
(&) :: a -> (a -> b) -> b
x & f = f x

handleNegativeLog (NegativeLog (x :: Double)) = "nl: " ++ show x
handleDivideByZero DivideByZero = "dz"

runner1 :: Double -> Double -> String
runner1 a b = do
  (show <$> testEther a b)
    & I.handleT handleNegativeLog
    & I.handle  handleDivideByZero

runner2 :: Double -> Double -> String
runner2 a b = do
  (show <$> testEther a b)
    & I.handleT handleDivideByZero
    & I.handle  handleNegativeLog

logDiv :: Double -> Double -> String
logDiv a b
  | b == 0    = "dz"
  | d  < 0    = "nl: " ++ show d
  | otherwise = show (log d)
  where
    d = a / b

test6 :: TestTree
test6 = testGroup "T6: Checked exceptions"
  [ testProperty "runner₁ works"
    $ \a b -> property
    $ runner1 a b == logDiv a b
  , testProperty "runner₂ works"
    $ \a b -> property
    $ runner2 a b == logDiv a b
  , testProperty "runner₁ == runner₂"
    $ \a b -> property
    $ runner1 a b == runner2 a b
  ]
