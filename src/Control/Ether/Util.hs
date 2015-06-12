module Control.Ether.Util
    ( liftCatch_ExceptT
    ) where

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.Except as Trans

liftCatch_ExceptT :: Sig.Catch e m (Either e' a) -> Sig.Catch e (Trans.ExceptT e' m) a
liftCatch_ExceptT catchE m h = Trans.ExceptT $ catchE (Trans.runExceptT m) (Trans.runExceptT . h)
