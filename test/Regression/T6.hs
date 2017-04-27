module Regression.T6 (test6) where

import Data.Function
import Ether

import Test.Tasty
import Test.Tasty.QuickCheck

data DivideByZero = DivideByZero
    deriving (Show)

data NegativeLog a = NegativeLog a
    deriving (Show)

testEther
  :: (Floating a, Ord a)
  => (MonadExcept' DivideByZero m, MonadExcept' (NegativeLog a) m)
  => a -> a -> m a
testEther a b = do
  when (b == 0) (throw' DivideByZero)
  let d = a / b
  when (d < 0) (throw' (NegativeLog d))
  return (log d)

handleNegativeLog (NegativeLog (x :: Double)) = "nl: " ++ show x
handleDivideByZero DivideByZero = "dz"

handleT' :: Functor m => (e -> a) -> ExceptT' e m a -> m a
handleT' h m = fmap (either h id) (runExceptT' m)

runner1 :: Double -> Double -> String
runner1 a b = do
  (show `fmap` testEther a b)
    & handleT' handleNegativeLog
    & handleT'  handleDivideByZero
    & runIdentity

runner2 :: Double -> Double -> String
runner2 a b = do
  (show `fmap` testEther a b)
    & handleT' handleDivideByZero
    & handleT' handleNegativeLog
    & runIdentity

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
