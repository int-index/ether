{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Ether.Util
    ( liftCatch_ExceptT
    , type (++)
    , MaybeToList
    ) where

import qualified Control.Monad.Signatures as Sig
import qualified Control.Monad.Trans.Except as Trans

type family (as :: [*]) ++ (bs :: [*]) :: [*] where
    '[] ++ bs = bs
    (a ': as) ++ bs = a ': (as ++ bs)

type family MaybeToList (mt :: Maybe k) :: [k] where
    MaybeToList 'Nothing = '[]
    MaybeToList ('Just t) = '[t]

liftCatch_ExceptT :: Sig.Catch e m (Either e' a) -> Sig.Catch e (Trans.ExceptT e' m) a
liftCatch_ExceptT catchE m h = Trans.ExceptT $ catchE (Trans.runExceptT m) (Trans.runExceptT . h)
