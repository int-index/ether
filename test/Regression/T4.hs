module Regression.T4 (test4) where

import Control.Ether.Abbr
import Control.Monad.Ether

import Test.Tasty
import Test.Tasty.QuickCheck

ethereal "RTag" "rTag"

testEther :: Ether '[RTag --> Int] m => m Int
testEther = ask rTag

runner r
  = flip (runReader  rTag) (r' :: Int)
  . flip (runReaderT rTag) (r  :: Int)
  . flip (runReaderT rTag) (fromIntegral r' :: Integer)
  where
    r' = negate r

test4 :: TestTree
test4 = testGroup "T4: Nested same-tag readers"
  [ testProperty "runner works"
    $ \r -> property
    $ runner r testEther == r
  ]
