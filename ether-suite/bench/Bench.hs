{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

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
import Control.DeepSeq

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
  :: ( E.MonadReader 1 Int m
     , E.MonadReader 2 Int m
     , E.MonadReader 3 Int m
     , E.MonadReader 4 Int m
     , E.MonadReader 5 Int m
     , E.MonadReader 6 Int m
     , E.MonadReader 7 Int m
     , E.MonadReader 8 Int m
     , E.MonadReader 9 Int m )
    => m ()
readerCombinerEther_sep_9 = rnf <$> sequenceA
  [ E.ask @1
  , E.ask @2
  , E.ask @3
  , E.ask @4
  , E.ask @5
  , E.ask @6
  , E.ask @7
  , E.ask @8
  , E.ask @9 ]
{-# NOINLINE readerCombinerEther_sep_9 #-}

stateCombinerEther_sep_9
  :: ( E.MonadState 1 Int m
     , E.MonadState 2 Int m
     , E.MonadState 3 Int m
     , E.MonadState 4 Int m
     , E.MonadState 5 Int m
     , E.MonadState 6 Int m
     , E.MonadState 7 Int m
     , E.MonadState 8 Int m
     , E.MonadState 9 Int m )
    => m ()
stateCombinerEther_sep_9 = do
  E.modify @1 id'
  E.modify @2 id'
  E.modify @3 id'
  E.modify @4 id'
  E.modify @5 id'
  E.modify @6 id'
  E.modify @7 id'
  E.modify @8 id'
  E.modify @9 id'
  rnf <$> sequenceA
    [ E.get @1
    , E.get @2
    , E.get @3
    , E.get @4
    , E.get @5
    , E.get @6
    , E.get @7
    , E.get @8
    , E.get @9 ]

run_readerCombinerEther_nested_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> ()
run_readerCombinerEther_nested_9
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
    $ readerCombinerEther_sep_9

run_readerCombinerEther_flatten_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> ()
run_readerCombinerEther_flatten_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    =
      E.F.runReader @'[1,2,3,4,5,6,7,8,9]
        readerCombinerEther_sep_9
          (P9 a9 a8 a7 a6 a5 a4 a3 a2 a1)


run_readerCombinerEther_flattenhalf_9 :: (Int, Int, Int, Int, Int, Int, Int, Int, Int) -> ()
run_readerCombinerEther_flattenhalf_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    = flip (E.F.runReader @'[1,2,3,4]) (P4 a9 a8 a7 a6)
    . flip (E.F.runReaderT @'[5,6,7,8,9]) (P5 a5 a4 a3 a2 a1)
    $ readerCombinerEther_sep_9

run_stateCombinerEther_flatten_9 ::
  (Int, Int, Int, Int, Int, Int, Int, Int, Int) ->
    ((), Product '[1,2,3,4,5,6,7,8,9] '[Int,Int,Int,Int,Int,Int,Int,Int,Int])
run_stateCombinerEther_flatten_9
  (a1, a2, a3, a4, a5, a6, a7, a8, a9)
    =
      E.F.runState
        stateCombinerEther_sep_9
          (P9 a9 a8 a7 a6 a5 a4 a3 a2 a1)

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
