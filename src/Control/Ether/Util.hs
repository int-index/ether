{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Ether.Util
    ( liftListen_WriterT
    , liftPass_WriterT
    , type (++)
    , MaybeToList
    , fmap
    ) where

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.Writer as Trans
import Prelude hiding (fmap)

#if __GLASGOW_HASKELL__ < 710
import qualified Control.Monad
#else
import qualified Prelude
#endif

type family (as :: [*]) ++ (bs :: [*]) :: [*] where
    '[] ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)

type family MaybeToList (mt :: Maybe k) :: [k] where
    MaybeToList 'Nothing = '[]
    MaybeToList ('Just t) = '[t]

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

#if __GLASGOW_HASKELL__ < 710
fmap :: Monad f => (a -> b) -> f a -> f b
fmap = Control.Monad.liftM 
#else
fmap :: Functor f => (a -> b) -> f a -> f b
fmap = Prelude.fmap 
#endif

{-# INLINE fmap #-}
