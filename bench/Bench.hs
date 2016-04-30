{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Criterion.Main
import qualified Control.Monad.Reader as M
import qualified Control.Monad.Ether.Reader as E
import qualified Control.Lens as L

readerSummatorMTL_flat_9
  :: M.MonadReader (Int, Int, Int, Int, Int, Int, Int, Int, Int) m
  => m String
readerSummatorMTL_flat_9 = do
  concat <$> sequenceA
    [ show <$> L.view L._1
    , show <$> L.view L._2
    , show <$> L.view L._3
    , show <$> L.view L._4
    , show <$> L.view L._5
    , show <$> L.view L._6
    , show <$> L.view L._7
    , show <$> L.view L._8
    , show <$> L.view L._9 ]
{-# NOINLINE readerSummatorMTL_flat_9 #-}

run_readerSummatorMTL_flat_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> String
run_readerSummatorMTL_flat_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    = M.runReader readerSummatorMTL_flat_9
    $ (a9, a8, a7, a6, a5, a4, a3, a2, a1)

readerSummatorEther_sep_9
  :: ( E.MonadReader 1 Int m
     , E.MonadReader 2 Int m
     , E.MonadReader 3 Int m
     , E.MonadReader 4 Int m
     , E.MonadReader 5 Int m
     , E.MonadReader 6 Int m
     , E.MonadReader 7 Int m
     , E.MonadReader 8 Int m
     , E.MonadReader 9 Int m )
    => m String
readerSummatorEther_sep_9 = concat <$> sequenceA
  [ show <$> E.ask @1
  , show <$> E.ask @2
  , show <$> E.ask @3
  , show <$> E.ask @4
  , show <$> E.ask @5
  , show <$> E.ask @6
  , show <$> E.ask @7
  , show <$> E.ask @8
  , show <$> E.ask @9 ]
{-# NOINLINE readerSummatorEther_sep_9 #-}

run_readerSummatorEther_nested_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> String
run_readerSummatorEther_nested_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    = flip (E.runReader  @9) a1
    . flip (E.runReaderT @8) a2
    . flip (E.runReaderT @7) a3
    . flip (E.runReaderT @6) a4
    . flip (E.runReaderT @5) a5
    . flip (E.runReaderT @4) a6
    . flip (E.runReaderT @3) a7
    . flip (E.runReaderT @2) a8
    . flip (E.runReaderT @1) a9
    $ readerSummatorEther_sep_9

tuple_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int)
tuple_9 = (1, -2, 3, -4, 5, -6, 7, -8, 9)

main :: IO ()
main = do
  defaultMain
    [ bench "readerSummatorMTL_flat_9" $ nf run_readerSummatorMTL_flat_9 tuple_9
    , bench "readerSummatorEther_nested_9" $ nf run_readerSummatorEther_nested_9 tuple_9
    ]
