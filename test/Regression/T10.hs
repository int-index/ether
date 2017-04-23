module Regression.T10 (test10) where

import Control.Applicative
import Data.Functor.Identity
import qualified Control.Monad.Ether.Implicit as I

import Test.Tasty
import Test.Tasty.QuickCheck

testEther :: Integer -> I.StateT Integer Maybe [Integer]
testEther m = range
  where
    range = liftA2 (:) yield (range <|> pure [])
    yield = I.stateT $ \n -> do
      I.guard (n <= m)
      Just (n, n + 1)

testEther' :: Integer -> I.State Integer [Integer]
testEther' m =
  I.stateT $ Identity . maybe ([], m + 1) id . I.runStateT (testEther m)

next1 :: Integer -> Integer -> Maybe Integer
next1 m n
  | n <= m    = Just (m + 1)
  | otherwise = Nothing

range1 :: Integer -> Integer -> Maybe [Integer]
range1 m n
  | n <= m    = Just [n..m]
  | otherwise = Nothing

test10 :: TestTree
test10 = testGroup "T10: Alternative instance"
  [ testProperty "execStateT works"
  $ \m n -> I.execStateT (testEther m) n == next1 m n
  , testProperty "evalStateT works"
  $ \m n -> I.evalStateT (testEther m) n == range1 m n
  , testProperty "execState works"
  $ \m n -> I.execState (testEther' m) n == m + 1
  , testProperty "evalState works"
  $ \m n -> I.evalState (testEther' m) n == [n..m]
  ]
