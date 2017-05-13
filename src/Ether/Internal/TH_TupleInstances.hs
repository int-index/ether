module Ether.Internal.TH_TupleInstances
  ( makeTupleInstancesTagsK
  , makeTupleInstancesTags
  , makeTupleInstancesHasLens
  ) where

import Data.Tagged
import Data.Traversable
import Data.List as List
import qualified Language.Haskell.TH as TH

import Ether.Internal.HasLens
import Ether.Internal.Tags
import Ether.Internal.TH_Utils

makeTupleInstancesTagsK :: TH.DecsQ
makeTupleInstancesTagsK = do
  for [2..tupCount] $ \n -> do
    let
      tupTy = List.foldl' TH.AppT (TH.ConT (TH.tupleTypeName n)) $
        (\k -> TH.ConT ''Tagged `TH.AppT`
          TH.VarT (tagName k) `TH.AppT`
          TH.VarT (varName k)) <$> [0..n-1]
      tagsList = List.foldr
        (\a b -> TH.PromotedConsT `TH.AppT` a `TH.AppT` b)
        TH.PromotedNilT
        (TH.AppT (TH.ConT ''KindOf) . TH.VarT . tagName <$> [0..n-1])
    return $
      TH.TySynInstD ''TagsK (TH.TySynEqn [tupTy] tagsList)

makeTupleInstancesTags :: TH.DecsQ
makeTupleInstancesTags = do
  for [2..tupCount] $ \n -> do
    let
      tupTy = List.foldl' TH.AppT (TH.ConT (TH.tupleTypeName n)) $
        (\k -> TH.ConT ''Tagged `TH.AppT`
          TH.VarT (tagName k) `TH.AppT`
          TH.VarT (varName k)) <$> [0..n-1]
      tagsList = List.foldr
        (\a b -> TH.PromotedT 'HCons `TH.AppT` a `TH.AppT` b)
        (TH.PromotedT 'HNil)
        (TH.VarT . tagName <$> [0..n-1])
    return $
      TH.TySynInstD ''Tags (TH.TySynEqn [tupTy] tagsList)

makeTupleInstancesHasLens :: [Int] -> TH.DecsQ
makeTupleInstancesHasLens range = List.concat <$> do
  for range $ \n ->
    for [0..n-1] $ \k -> do
      let
        tag = TH.mkName "tag"
        prev = varName <$> [0..k-1]
        cur  = varName k
        next = varName <$> [k+1..n-1]
        tupTy = foldl TH.AppT (TH.ConT (TH.tupleTypeName n))
          ( map TH.VarT prev ++
            [TH.ConT ''Tagged `TH.AppT` TH.VarT tag `TH.AppT` TH.VarT cur] ++
            map TH.VarT next )
      let
        cur' = TH.mkName "x"
        f = TH.mkName "f"
      return $
        TH.InstanceD Nothing []
          (TH.ConT ''HasLens `TH.AppT` TH.VarT tag `TH.AppT` tupTy `TH.AppT` TH.VarT cur)
          [ TH.FunD 'lensOf
              [ TH.Clause
                  [TH.VarP f, TH.TupP
                    ( map TH.VarP prev ++
                      [TH.ConP 'Tagged [TH.VarP cur]] ++
                      map TH.VarP next )]
                  (TH.NormalB $
                     TH.VarE 'fmap `TH.AppE`
                       (TH.LamE [TH.VarP cur']
                         (TH.TupE
                          ( map TH.VarE prev ++
                            [TH.ConE 'Tagged `TH.AppE` TH.VarE cur'] ++
                            map TH.VarE next ))) `TH.AppE`
                       (TH.VarE f `TH.AppE` TH.VarE cur) )
                  [] ] ]
