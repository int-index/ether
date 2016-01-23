module Main where

import Control.Monad

import Control.Monad.Ether
import Control.Ether.Abbr

import qualified Control.Monad.Reader as T
import qualified Control.Monad.State  as T

import Test.Tasty

import Regression.T1
import Regression.T2
import Regression.T3
import Regression.T4
import Regression.T5
import Regression.T6
import Regression.T7
import Regression.T8

ethereal "Foo" "foo"
ethereal "Bar" "bar"

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Ether"
  [ test1
  , test2
  , test3
  , test4
  , test5
  , test6
  , test7
  , test8
  ]

wrapState_f :: T.MonadState Int m => m String
wrapState_f = liftM show T.get

wrapState_g :: T.MonadState Bool m => m String
wrapState_g = liftM show T.get

wrapState_useboth :: Ether '[Foo <-> Int, Bar <-> Bool] m => m String
wrapState_useboth = do
    a <- tagAttach foo wrapState_f
    b <- tagAttach bar wrapState_g
    return (a ++ b)

wrapStateCore :: Int -> Bool -> String
wrapStateCore int bool = evalState foo (evalStateT bar wrapState_useboth bool) int

wrapStateBad1_g :: MonadState Foo Int m => m ()
wrapStateBad1_g = modify foo (*100)

wrapStateBad1_useboth :: MonadState Foo Int m => m String
wrapStateBad1_useboth = do
    wrapStateBad1_g
    tagAttach foo wrapState_f

wrapStateBad1 :: Int -> String
wrapStateBad1 = evalState foo wrapStateBad1_useboth

wrapStateBad2 :: (T.MonadState Int m , MonadState Foo Int m) => m Int
wrapStateBad2 = do
    modify foo (*100)
    T.get

wrapStateBad2Core :: Int -> Int
wrapStateBad2Core = evalState foo (tagAttach foo wrapStateBad2)

wrapReaderCore :: Int -> Int
wrapReaderCore = runReader foo (tagAttach foo (liftM2 (+) T.ask (ask foo)))
