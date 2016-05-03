{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  , dispatchT
  , runDispatchT
  , redispatch
  -- * Dispatch types and functions
  , K_TagAttach(..)
  , tagAttach
  , K_TagReplace(..)
  , tagReplace
  ) where

import Control.Applicative
import Control.Monad (MonadPlus)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor, MMonad)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)

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

import Control.Monad.Ether.Reader as A
import Control.Monad.Ether.State.Common as A
import Control.Monad.Ether.Except as A
import Control.Monad.Ether.Writer as A

import qualified Control.Monad.Cont.Class    as Class
import qualified Control.Monad.Reader.Class  as Class
import qualified Control.Monad.State.Class   as Class
import qualified Control.Monad.Writer.Class  as Class
import qualified Control.Monad.Error.Class   as Class

import GHC.Generics (Generic)
import Data.Coerce (coerce)

-- | Wrap a monad to change its tags. Under the hood this is
-- simply 'Trans.IdentityT', all the work is happening on the type level.
newtype DispatchT dp m a = DispatchT (Trans.IdentityT m a)
  deriving
    ( Generic
    , Functor, Applicative, Alternative, Monad, MonadPlus
    , MonadFix, MonadTrans, MonadIO, MFunctor, MMonad
    , MonadThrow, MonadCatch, MonadMask )

-- | Type-restricted 'coerce'.
pack :: forall dp m a . Trans.IdentityT m a -> DispatchT dp m a
pack = coerce
{-# INLINE pack #-}

-- | Type-restricted 'coerce'.
unpack :: forall dp m a . DispatchT dp m a -> Trans.IdentityT m a
unpack = coerce
{-# INLINE unpack #-}

-- | Type-restricted 'coerce'.
dispatchT :: forall dp m a . m a -> DispatchT dp m a
dispatchT = coerce
{-# INLINE dispatchT #-}

-- | Type-restricted 'coerce'.
runDispatchT :: forall dp m a . DispatchT dp m a -> m a
runDispatchT = coerce
{-# INLINE runDispatchT #-}

-- | Type-restricted 'coerce'.
redispatch :: forall dp dp' m a . DispatchT dp m a -> DispatchT dp' m a
redispatch = coerce
{-# INLINE redispatch #-}

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

-- Instances for mtl classes

instance {-# OVERLAPPABLE #-}
    ( Class.MonadCont m
    , Monad m
    ) => Class.MonadCont (DispatchT dp m)
  where
    callCC = Lift.liftCallCC' Class.callCC

instance {-# OVERLAPPABLE #-}
    ( Class.MonadReader r m
    , Monad m
    ) => Class.MonadReader r (DispatchT dp m)
  where
    ask = lift Class.ask
    local = Lift.liftLocal Class.ask Class.local
    reader = lift . Class.reader

instance {-# OVERLAPPABLE #-}
    ( Class.MonadState s m
    , Monad m
    ) => Class.MonadState s (DispatchT dp m)
  where
    get = lift Class.get
    put = lift . Class.put
    state = lift . Class.state

instance {-# OVERLAPPABLE #-}
    ( Class.MonadWriter w m
    , Monad m
    ) => Class.MonadWriter w (DispatchT dp m)
  where
    writer = lift . Class.writer
    tell   = lift . Class.tell
    listen = Lift.liftListen Class.listen
    pass   = Lift.liftPass Class.pass

instance {-# OVERLAPPABLE #-}
    ( Class.MonadError e m
    , Monad m
    ) => Class.MonadError e (DispatchT dp m)
  where
    throwError = lift . Class.throwError
    catchError = Lift.liftCatch Class.catchError


-- TagAttach

-- | Encode type-level information for 'tagAttach'.
data K_TagAttach t = TagAttach t

type DispatchTagAttachT t = DispatchT (TagAttach t)

-- | Attach a tag to untagged transformers.
tagAttach :: forall tag m a . DispatchTagAttachT tag m a -> m a
tagAttach = runDispatchT

instance MonadReader tag r m
      => Class.MonadReader r (DispatchTagAttachT tag m) where
  ask   = Lift.lift (A.ask @tag)
  local = Lift.liftLocal (A.ask @tag) (A.local @tag)

instance MonadState tag s m
      => Class.MonadState s (DispatchTagAttachT tag m) where
  get = Lift.lift (A.get @tag)
  put = Lift.lift . A.put @tag

instance MonadExcept tag e m
      => Class.MonadError e (DispatchTagAttachT tag m) where
  throwError = Lift.lift . A.throw @tag
  catchError = Lift.liftCatch (A.catch @tag)

instance MonadWriter tag w m
      => Class.MonadWriter w (DispatchTagAttachT tag m) where
  writer = Lift.lift . A.writer @tag
  tell   = Lift.lift . A.tell @tag
  listen = Lift.liftListen (A.listen @tag)
  pass   = Lift.liftPass (A.pass @tag)


-- TagReplace

-- | Encode type-level information for 'tagReplace'.
data K_TagReplace tOld tNew = TagReplace tOld tNew

type DispatchTagReplaceT tOld tNew = DispatchT (TagReplace tOld tNew)

-- | Replace a tag with another tag.
tagReplace :: forall tOld tNew m a . DispatchTagReplaceT tOld tNew m a -> m a
tagReplace = runDispatchT

instance MonadReader tNew r m
      => MonadReader tOld r (DispatchTagReplaceT tOld tNew m) where
  ask   _ = Lift.lift (A.ask @tNew)
  local _ = Lift.liftLocal (A.ask @tNew) (A.local @tNew)

instance MonadState tNew s m
      => MonadState tOld s (DispatchTagReplaceT tOld tNew m) where
  get _ = Lift.lift (A.get @tNew)
  put _ = Lift.lift . A.put @tNew

instance MonadExcept tNew e m
      => MonadExcept tOld e (DispatchTagReplaceT tOld tNew m) where
  throw _ = Lift.lift . A.throw @tNew
  catch _ = Lift.liftCatch (A.catch @tNew)

instance MonadWriter tNew w m
      => MonadWriter tOld w (DispatchTagReplaceT tOld tNew m) where
  writer _ = Lift.lift . A.writer @tNew
  tell   _ = Lift.lift . A.tell @tNew
  listen _ = Lift.liftListen (A.listen @tNew)
  pass   _ = Lift.liftPass (A.pass @tNew)
