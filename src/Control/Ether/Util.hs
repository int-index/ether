{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Ether.Util
    ( liftCatch_ExceptT
    , liftListen_WriterT
    , liftPass_WriterT
    , type (++)
    , MaybeToList
    ) where

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.Except as Trans
import qualified Control.Monad.Trans.Writer as Trans

type family (as :: [*]) ++ (bs :: [*]) :: [*] where
    '[] ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)

type family MaybeToList (mt :: Maybe k) :: [k] where
    MaybeToList 'Nothing = '[]
    MaybeToList ('Just t) = '[t]

liftCatch_ExceptT :: Sig.Catch e m (Either e' a) -> Sig.Catch e (Trans.ExceptT e' m) a
liftCatch_ExceptT catchE m h = Trans.ExceptT $ catchE (Trans.runExceptT m) (Trans.runExceptT . h)

-- TODO: Not sure if correct
liftListen_WriterT :: Monad m => Sig.Listen w' m (a, w) -> Sig.Listen w' (Trans.WriterT w m) a
liftListen_WriterT listen m = Trans.WriterT $ do
    ~((a, w'), w) <- listen (Trans.runWriterT m)
    return ((a, w), w')

-- TODO: Not sure if correct
liftPass_WriterT :: Monad m => Sig.Pass w' m (a, w) -> Sig.Pass w' (Trans.WriterT w m) a
liftPass_WriterT pass m = Trans.WriterT $ pass $ do
    ~((a, f), w) <- Trans.runWriterT m
    return ((a, w), f)
