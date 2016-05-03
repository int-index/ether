module Regression.T9 (test9) where

import Control.Ether.Abbr
import Control.Monad.Ether

import Test.Tasty
import Test.Tasty.QuickCheck

data Foo
data Bar

testEther1 :: Ether '[Foo <-> Int] m => m String
testEther1 = do
  modify @Foo negate
  gets @Foo show

testEther2 :: Ether '[Bar <-> Int] m => m String
testEther2 = tagReplace @Foo @Bar testEther1

testEther
  :: Ether '[Foo <-> Int, Bar <-> Int] m
  => m String
testEther = do
  a <- testEther1
  b <- testEther2
  return (a ++ b)

model :: Int -> Int -> String
model a b = show (negate a) ++ show (negate b)

runner1 a b
  = flip (evalState  @Foo) a
  . flip (evalStateT @Bar) b

runner2 a b
  = flip (evalState  @Bar) b
  . flip (evalStateT @Foo) a

test9 :: TestTree
test9 = testGroup "T9: Tag replacement"
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
