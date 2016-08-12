{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Criterion.Main
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State as M
import qualified Control.Monad.Ether.Reader as E
import qualified Control.Monad.Ether.State as E
import qualified Control.Monad.Ether.Reader.Flatten as E.F
import qualified Control.Monad.Ether.State.Flatten as E.F
import Control.Ether.Flatten
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

stateSummatorMTL_flat_9
  :: M.MonadState (Int, Int, Int, Int, Int, Int, Int, Int, Int) m
  => m String
stateSummatorMTL_flat_9 = do
  M.modify (L.over L._1 (+1))
  M.modify (L.over L._2 (+2))
  M.modify (L.over L._3 (+3))
  M.modify (L.over L._4 (+4))
  M.modify (L.over L._5 (+5))
  M.modify (L.over L._6 (+6))
  M.modify (L.over L._7 (+7))
  M.modify (L.over L._8 (+8))
  M.modify (L.over L._9 (+9))
  concat <$> sequenceA
    [ show <$> L.use L._1
    , show <$> L.use L._2
    , show <$> L.use L._3
    , show <$> L.use L._4
    , show <$> L.use L._5
    , show <$> L.use L._6
    , show <$> L.use L._7
    , show <$> L.use L._8
    , show <$> L.use L._9 ]
{-# NOINLINE stateSummatorMTL_flat_9 #-}

run_readerSummatorMTL_flat_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> String
run_readerSummatorMTL_flat_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    M.runReader readerSummatorMTL_flat_9 (a9, a8, a7, a6, a5, a4, a3, a2, a1)

run_stateSummatorMTL_flat_9 ::
  (Int, Int, Int, Int, Int, Int, Int, Int, Int) ->
    (String, (Int, Int, Int, Int, Int, Int, Int, Int, Int))
run_stateSummatorMTL_flat_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    M.runState stateSummatorMTL_flat_9 (a9, a8, a7, a6, a5, a4, a3, a2, a1)

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

stateSummatorEther_sep_9
  :: ( E.MonadState 1 Int m
     , E.MonadState 2 Int m
     , E.MonadState 3 Int m
     , E.MonadState 4 Int m
     , E.MonadState 5 Int m
     , E.MonadState 6 Int m
     , E.MonadState 7 Int m
     , E.MonadState 8 Int m
     , E.MonadState 9 Int m )
    => m String
stateSummatorEther_sep_9 = do
  E.modify @1 (+1)
  E.modify @2 (+2)
  E.modify @3 (+3)
  E.modify @4 (+4)
  E.modify @5 (+5)
  E.modify @6 (+6)
  E.modify @7 (+7)
  E.modify @8 (+8)
  E.modify @9 (+9)
  concat <$> sequenceA
    [ show <$> E.get @1
    , show <$> E.get @2
    , show <$> E.get @3
    , show <$> E.get @4
    , show <$> E.get @5
    , show <$> E.get @6
    , show <$> E.get @7
    , show <$> E.get @8
    , show <$> E.get @9 ]

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

run_readerSummatorEther_flatten_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> String
run_readerSummatorEther_flatten_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    =
      E.F.runReader
        readerSummatorEther_sep_9
          ( load @1 a9 <+> load @2 a8 <+> load @3 a7 <+> load @4 a6 <+>
            load @5 a5 <+> load @6 a4 <+> load @7 a3 <+> load @8 a2 <+>
            load @9 a1 )

run_readerSummatorEther_flattenhalf_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> String
run_readerSummatorEther_flattenhalf_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    = flip E.F.runReader (load @1 a9 <+> load @2 a8 <+> load @3 a7 <+> load @4 a6)
    . flip E.F.runReaderT (load @5 a5 <+> load @6 a4 <+> load @7 a3 <+> load @8 a2 <+> load @9 a1)
    $ readerSummatorEther_sep_9

run_stateSummatorEther_flatten_9 ::
  (Int, Int, Int, Int, Int, Int, Int, Int, Int) ->
    (String, Product '[1,2,3,4,5,6,7,8,9] '[Int,Int,Int,Int,Int,Int,Int,Int,Int])
run_stateSummatorEther_flatten_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    =
      E.F.runState
        stateSummatorEther_sep_9
          ( load @1 a9 <+> load @2 a8 <+> load @3 a7 <+> load @4 a6 <+>
            load @5 a5 <+> load @6 a4 <+> load @7 a3 <+> load @8 a2 <+>
            load @9 a1 )

tuple_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int)
tuple_9 = (1, -2, 3, -4, 5, -6, 7, -8, 9)

main :: IO ()
main = do
  defaultMain
    [ bench "readerSummatorMTL_flat_9" $ nf run_readerSummatorMTL_flat_9 tuple_9
    , bench "readerSummatorEther_nested_9" $ nf run_readerSummatorEther_nested_9 tuple_9
    , bench "readerSummatorEther_flatten_9" $ nf run_readerSummatorEther_flatten_9 tuple_9
    , bench "readerSummatorEther_flattenhalf_9" $
        nf run_readerSummatorEther_flattenhalf_9 tuple_9
    , bench "stateSummatorMTL_flat_9" $ nf run_stateSummatorMTL_flat_9 tuple_9
    , bench "stateSummatorEther_flatten_9" $ nf run_stateSummatorEther_flatten_9 tuple_9
    ]
