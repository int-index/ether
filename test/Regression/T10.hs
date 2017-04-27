module Regression.T10 (test10) where

import Control.Applicative
import Data.Functor.Identity

import Ether

import Test.Tasty
import Test.Tasty.QuickCheck

testEther :: Integer -> StateT' Integer Maybe [Integer]
testEther m = range
  where
    range = liftA2 (:) yield (range <|> pure [])
    yield = stateT' $ \n -> do
      guard (n <= m)
      Just (n, n + 1)

testEther' :: Integer -> State' Integer [Integer]
testEther' m =
  stateT' $ Identity . maybe ([], m + 1) id . runStateT' (testEther m)

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
  $ \m n -> execStateT' (testEther m) n == next1 m n
  , testProperty "evalStateT works"
  $ \m n -> evalStateT' (testEther m) n == range1 m n
  , testProperty "execState works"
  $ \m n -> execState' (testEther' m) n == m + 1
  , testProperty "evalState works"
  $ \m n -> evalState' (testEther' m) n == [n..m]
  ]
