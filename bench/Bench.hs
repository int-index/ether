{-# OPTIONS -fno-warn-partial-type-signatures #-}

module Main (main) where

import Control.DeepSeq
import qualified Control.Lens as L
import qualified Control.Monad.Reader as M
import qualified Control.Monad.State as M
import Criterion.Main
import Ether
import TupleInstances ()

id' :: a -> a
id' = id
{-# NOINLINE id' #-}

readerCombinerMTL_flat_9
  :: M.MonadReader (Int, Int, Int, Int, Int, Int, Int, Int, Int) m
  => m ()
readerCombinerMTL_flat_9 = do
  rnf <$> sequenceA
    [ L.view L._1
    , L.view L._2
    , L.view L._3
    , L.view L._4
    , L.view L._5
    , L.view L._6
    , L.view L._7
    , L.view L._8
    , L.view L._9 ]
{-# NOINLINE readerCombinerMTL_flat_9 #-}

stateCombinerMTL_flat_9
  :: M.MonadState (Int, Int, Int, Int, Int, Int, Int, Int, Int) m
  => m ()
stateCombinerMTL_flat_9 = do
  M.modify (L.over L._1 id')
  M.modify (L.over L._2 id')
  M.modify (L.over L._3 id')
  M.modify (L.over L._4 id')
  M.modify (L.over L._5 id')
  M.modify (L.over L._6 id')
  M.modify (L.over L._7 id')
  M.modify (L.over L._8 id')
  M.modify (L.over L._9 id')
  rnf <$> sequenceA
    [ L.use L._1
    , L.use L._2
    , L.use L._3
    , L.use L._4
    , L.use L._5
    , L.use L._6
    , L.use L._7
    , L.use L._8
    , L.use L._9 ]
{-# NOINLINE stateCombinerMTL_flat_9 #-}

run_readerCombinerMTL_flat_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> ()
run_readerCombinerMTL_flat_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    M.runReader readerCombinerMTL_flat_9 (a9, a8, a7, a6, a5, a4, a3, a2, a1)

run_stateCombinerMTL_flat_9 ::
  (Int, Int, Int, Int, Int, Int, Int, Int, Int) ->
    ((), (Int, Int, Int, Int, Int, Int, Int, Int, Int))
run_stateCombinerMTL_flat_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    M.runState stateCombinerMTL_flat_9 (a9, a8, a7, a6, a5, a4, a3, a2, a1)

readerCombinerEther_sep_9
  :: ( MonadReader 1 Int m
     , MonadReader 2 Int m
     , MonadReader 3 Int m
     , MonadReader 4 Int m
     , MonadReader 5 Int m
     , MonadReader 6 Int m
     , MonadReader 7 Int m
     , MonadReader 8 Int m
     , MonadReader 9 Int m )
    => m ()
readerCombinerEther_sep_9 = rnf <$> sequenceA
  [ ask @1
  , ask @2
  , ask @3
  , ask @4
  , ask @5
  , ask @6
  , ask @7
  , ask @8
  , ask @9 ]
{-# NOINLINE readerCombinerEther_sep_9 #-}

stateCombinerEther_sep_9
  :: ( MonadState 1 Int m
     , MonadState 2 Int m
     , MonadState 3 Int m
     , MonadState 4 Int m
     , MonadState 5 Int m
     , MonadState 6 Int m
     , MonadState 7 Int m
     , MonadState 8 Int m
     , MonadState 9 Int m )
    => m ()
stateCombinerEther_sep_9 = do
  modify @1 id'
  modify @2 id'
  modify @3 id'
  modify @4 id'
  modify @5 id'
  modify @6 id'
  modify @7 id'
  modify @8 id'
  modify @9 id'
  rnf <$> sequenceA
    [ get @1
    , get @2
    , get @3
    , get @4
    , get @5
    , get @6
    , get @7
    , get @8
    , get @9 ]
{-# NOINLINE stateCombinerEther_sep_9 #-}

run_readerCombinerEther_nested_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> ()
run_readerCombinerEther_nested_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    = flip (runReader  @9) a1
    . flip (runReaderT @8) a2
    . flip (runReaderT @7) a3
    . flip (runReaderT @6) a4
    . flip (runReaderT @5) a5
    . flip (runReaderT @4) a6
    . flip (runReaderT @3) a7
    . flip (runReaderT @2) a8
    . flip (runReaderT @1) a9
    $ readerCombinerEther_sep_9

run_readerCombinerEther_flatten_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> ()
run_readerCombinerEther_flatten_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    =
      runReaders
        readerCombinerEther_sep_9
          ( Tagged @1 a9,
            Tagged @2 a8,
            Tagged @3 a7,
            Tagged @4 a6,
            Tagged @5 a5,
            Tagged @6 a4,
            Tagged @7 a3,
            Tagged @8 a2,
            Tagged @9 a1 )

run_readerCombinerEther_flattenhalf_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> ()
run_readerCombinerEther_flattenhalf_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9) =
    flip runReaders
      ( Tagged @1 a9,
        Tagged @2 a8,
        Tagged @3 a7,
        Tagged @4 a6 ) .
    flip runReadersT
      ( Tagged @5 a5,
        Tagged @6 a4,
        Tagged @7 a3,
        Tagged @8 a2,
        Tagged @9 a1 ) $
    readerCombinerEther_sep_9

run_stateCombinerEther_flatten_9 ::
  (Int, Int, Int, Int, Int, Int, Int, Int, Int) ->
    ((), _)
run_stateCombinerEther_flatten_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    =
      runStates
        stateCombinerEther_sep_9
          ( Tagged @1 a9,
            Tagged @2 a8,
            Tagged @3 a7,
            Tagged @4 a6,
            Tagged @5 a5,
            Tagged @6 a4,
            Tagged @7 a3,
            Tagged @8 a2,
            Tagged @9 a1 )

tuple_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int)
tuple_9 = (1, -2, 3, -4, 5, -6, 7, -8, 9)

main :: IO ()
main = do
  defaultMain
    [ bench "readerCombinerMTL_flat_9" $ nf run_readerCombinerMTL_flat_9 tuple_9
    , bench "readerCombinerEther_nested_9" $ nf run_readerCombinerEther_nested_9 tuple_9
    , bench "readerCombinerEther_flatten_9" $ nf run_readerCombinerEther_flatten_9 tuple_9
    , bench "readerCombinerEther_flattenhalf_9" $
        nf run_readerCombinerEther_flattenhalf_9 tuple_9
    , bench "stateCombinerMTL_flat_9" $ nf run_stateCombinerMTL_flat_9 tuple_9
    , bench "stateCombinerEther_flatten_9" $ nf run_stateCombinerEther_flatten_9 tuple_9
    ]
