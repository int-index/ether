{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
#endif

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}

{- |

Type-level machinery to manipulate constraints on the monad
transformer stack.

Out of the box it provides the following dispatch strategies:

* 'tagAttach' to use functions defined using untagged monad classes
  as if they were defined using tagged ones.

* 'tagReplace' to use functions defined using one tag
  as if they were defined using another one.

> import qualified Control.Monad.State as T
> import Control.Ether.TH (ethereal)
> import Control.Monad.Ether.State (MonadState)
> import Control.Monad.Trans.Ether.Dispatch (tagAttach, tagDispatch)
>
> ethereal "Foo" "foo"
> ethereal "Bar" "bar"
>
> f :: T.MonadState Int m => m String
> f = fmap show T.get
>
> g :: MonadState Foo Int m => m String
> g = tagAttach foo f
>
> h :: MonadState Bar Int m => m String
> h = tagReplace foo bar g

-}

module Control.Monad.Trans.Ether.Dispatch
  (
  -- * The DispatchT monad transformer
    DispatchT
  -- * Dispatch types and functions
  , K_TagAttach(..)
  , K_TagReplace(..)
  , tagAttach
  , tagReplace
  ) where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor, MMonad)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import GHC.Generics (Generic)
import Data.Coerce (coerce)
import Data.Proxy
import GHC.Prim (Proxy#, proxy#)

import qualified Control.Monad.Base as MB
import qualified Control.Monad.Trans.Control as MC
import qualified Control.Monad.Trans.Identity as Trans

import qualified Control.Monad.Trans.Lift.StT    as Lift
import qualified Control.Monad.Trans.Lift.Local  as Lift
import qualified Control.Monad.Trans.Lift.Catch  as Lift
import qualified Control.Monad.Trans.Lift.Listen as Lift
import qualified Control.Monad.Trans.Lift.Pass   as Lift
import qualified Control.Monad.Trans.Lift.CallCC as Lift

import Control.Monad.Ether.Reader.Class
import Control.Monad.Ether.State.Class
import Control.Monad.Ether.Except.Class
import Control.Monad.Ether.Writer.Class

import qualified Control.Monad.Reader as Class
import qualified Control.Monad.State  as Class
import qualified Control.Monad.Except as Class
import qualified Control.Monad.Writer as Class

-- | Encode type-level information for 'tagAttach'.
data K_TagAttach t = TagAttach t

-- | Encode type-level information for 'tagReplace'.
data K_TagReplace tOld tNew = TagReplace tOld tNew

-- | Wrap a monad to change its tags. Under the hood this is
-- simply 'Trans.IdentityT', all the work is happening on the type level.
newtype DispatchT dp m a = DispatchT (Trans.IdentityT m a)
  deriving
    ( Generic
    , Functor, Applicative, Alternative, Monad, MonadPlus
    , MonadFix, MonadTrans, MonadIO, MFunctor, MMonad
    , MonadThrow, MonadCatch, MonadMask )

type DispatchTagAttachT t = DispatchT (TagAttach t)
type DispatchTagReplaceT tOld tNew = DispatchT (TagReplace tOld tNew)

-- | Type-restricted 'coerce'.
pack :: Trans.IdentityT m a -> DispatchT dp m a
pack = coerce

-- | Type-restricted 'coerce'.
unpack :: DispatchT dp m a -> Trans.IdentityT m a
unpack = coerce

instance MB.MonadBase b m => MB.MonadBase b (DispatchT dp m) where
  liftBase = MB.liftBaseDefault

instance MC.MonadTransControl (DispatchT dp) where
  type StT (DispatchT dp) a = MC.StT Trans.IdentityT a
  liftWith = MC.defaultLiftWith pack unpack
  restoreT = MC.defaultRestoreT pack

instance MC.MonadBaseControl b m => MC.MonadBaseControl b (DispatchT dp m) where
  type StM (DispatchT dp m) a = MC.ComposeSt (DispatchT dp) m a
  liftBaseWith = MC.defaultLiftBaseWith
  restoreM = MC.defaultRestoreM

type instance Lift.StT (DispatchT dp) a = MC.StT (DispatchT dp) a

instance Lift.LiftLocal (DispatchT dp) where
  liftLocal = Lift.defaultLiftLocal pack unpack

instance Lift.LiftCatch (DispatchT dp) where
  liftCatch = Lift.defaultLiftCatch pack unpack

instance Lift.LiftListen (DispatchT dp) where
  liftListen = Lift.defaultLiftListen pack unpack

instance Lift.LiftPass (DispatchT dp) where
  liftPass = Lift.defaultLiftPass pack unpack

instance Lift.LiftCallCC (DispatchT dp) where
  liftCallCC  = Lift.defaultLiftCallCC  pack unpack
  liftCallCC' = Lift.defaultLiftCallCC' pack unpack

-- | Attach a tag to untagged transformers.
tagAttach :: proxy t -> DispatchTagAttachT t m a -> m a
tagAttach _ = coerce

-- | Replace a tag with another tag.
tagReplace
  :: proxy tOld
  -> proxy tNew
  -> DispatchTagReplaceT tOld tNew m a
  -> m a
tagReplace _ _ = coerce


-- TagAttach instances

instance MonadReader tag r m
      => Class.MonadReader r (DispatchTagAttachT tag m) where
  ask   = let t = proxy# :: Proxy# tag in Lift.lift (ask t)
  local = let t = proxy# :: Proxy# tag in Lift.liftLocal (ask t) (local t)

instance MonadState tag s m
      => Class.MonadState s (DispatchTagAttachT tag m) where
  get = let t = Proxy :: Proxy tag in Lift.lift (get t)
  put = let t = Proxy :: Proxy tag in Lift.lift . put t

instance MonadExcept tag e m
      => Class.MonadError e (DispatchTagAttachT tag m) where
  throwError = let t = proxy# :: Proxy# tag in Lift.lift . throw t
  catchError = let t = proxy# :: Proxy# tag in Lift.liftCatch (catch t)

instance MonadWriter tag w m
      => Class.MonadWriter w (DispatchTagAttachT tag m) where
  writer = let t = proxy# :: Proxy# tag in Lift.lift . writer t
  tell   = let t = proxy# :: Proxy# tag in Lift.lift . tell t
  listen = let t = proxy# :: Proxy# tag in Lift.liftListen (listen t)
  pass   = let t = proxy# :: Proxy# tag in Lift.liftPass (pass t)


-- TagReplace instances

instance MonadReader tNew r m
      => MonadReader tOld r (DispatchTagReplaceT tOld tNew m) where
  ask   _ = let t = proxy# :: Proxy# tNew in Lift.lift (ask t)
  local _ = let t = proxy# :: Proxy# tNew in Lift.liftLocal (ask t) (local t)

instance MonadState tNew s m
      => MonadState tOld s (DispatchTagReplaceT tOld tNew m) where
  get _ = let t = Proxy :: Proxy tNew in Lift.lift (get t)
  put _ = let t = Proxy :: Proxy tNew in Lift.lift . put t

instance MonadExcept tNew e m
      => MonadExcept tOld e (DispatchTagReplaceT tOld tNew m) where
  throw _ = let t = proxy# :: Proxy# tNew in Lift.lift . throw t
  catch _ = let t = proxy# :: Proxy# tNew in Lift.liftCatch (catch t)

instance MonadWriter tNew w m
      => MonadWriter tOld w (DispatchTagReplaceT tOld tNew m) where
  writer _ = let t = proxy# :: Proxy# tNew in Lift.lift . writer t
  tell   _ = let t = proxy# :: Proxy# tNew in Lift.lift . tell t
  listen _ = let t = proxy# :: Proxy# tNew in Lift.liftListen (listen t)
  pass   _ = let t = proxy# :: Proxy# tNew in Lift.liftPass (pass t)
