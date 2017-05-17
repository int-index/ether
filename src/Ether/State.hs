module Ether.State
    (
    -- * MonadState class
      MonadState
    , get
    , put
    , state
    , modify
    , gets
    -- * The State monad
    , State
    , runState
    , evalState
    , execState
    -- * The StateT monad transformer
    , StateT
    , stateT
    , runStateT
    , evalStateT
    , execStateT
    -- * The State monad (lazy)
    , LazyState
    , runLazyState
    , evalLazyState
    , execLazyState
    -- * The StateT monad transformer (lazy)
    , LazyStateT
    , lazyStateT
    , runLazyStateT
    , evalLazyStateT
    , execLazyStateT
    -- * The State monad (flattened)
    , States
    , runStates
    -- * The StateT monad transformer (flattened)
    , StatesT
    , runStatesT
    -- * MonadState class (implicit)
    , MonadState'
    , get'
    , put'
    , state'
    , modify'
    , gets'
    -- * The State monad (implicit)
    , State'
    , runState'
    , evalState'
    , execState'
    -- * The StateT monad transformer (implicit)
    , StateT'
    , stateT'
    , runStateT'
    , evalStateT'
    , execStateT'
    -- * The State monad (lazy, implicit)
    , LazyState'
    , runLazyState'
    , evalLazyState'
    , execLazyState'
    -- * The StateT monad transformer (lazy, implicit)
    , LazyStateT'
    , lazyStateT'
    , runLazyStateT'
    , evalLazyStateT'
    , execLazyStateT'
    -- * Zoom
    , ZoomT
    , zoom
    -- * Internal labels
    , TAGGED
    , STATE
    , STATES
    , ZOOM
    ) where

import qualified Control.Monad.State.Class as T
import qualified Control.Monad.Trans as Lift
import Control.Monad.Trans.Identity
import qualified Control.Monad.Trans.State.Lazy as T.Lazy
import qualified Control.Monad.Trans.State.Strict as T.Strict
import Data.Coerce
import Data.Functor.Identity
import Data.Kind
import Data.Proxy
import Data.Reflection

import Ether.Internal
import Ether.TaggedTrans

class Monad m => MonadState tag s m | m tag -> s where

    {-# MINIMAL state | get, put #-}

    -- | Return the state from the internals of the monad.
    get :: m s
    get = state @tag (\s -> (s, s))

    -- | Replace the state inside the monad.
    put :: s -> m ()
    put s = state @tag (\_ -> ((), s))

    -- | Embed a simple state action into the monad.
    state :: (s -> (a, s)) -> m a
    state f = do
      s <- get @tag
      let ~(a, s') = f s
      put @tag s'
      return a

instance {-# OVERLAPPABLE #-}
    ( Lift.MonadTrans t
    , Monad (t m)
    , MonadState tag s m
    ) => MonadState tag s (t m)
  where
    get = Lift.lift (get @tag)
    put = Lift.lift . put @tag
    state = Lift.lift . state @tag

instance {-# OVERLAPPABLE #-}
    ( Monad (trans m)
    , MonadState tag s (TaggedTrans effs trans m)
    ) => MonadState tag s (TaggedTrans (eff ': effs) trans (m :: Type -> Type))
  where

    get =
      (coerce ::
        TaggedTrans         effs  trans m s ->
        TaggedTrans (eff ': effs) trans m s)
      (get @tag)

    put =
      (coerce ::
        (s -> TaggedTrans         effs  trans m ()) ->
        (s -> TaggedTrans (eff ': effs) trans m ()))
      (put @tag)

    state =
      (coerce :: forall a .
        ((s -> (a, s)) -> TaggedTrans         effs  trans m a) ->
        ((s -> (a, s)) -> TaggedTrans (eff ': effs) trans m a))
      (state @tag)

-- | Modifies the state inside a state monad.
modify :: forall tag s m . MonadState tag s m => (s -> s) -> m ()
modify f = state @tag (\s -> ((), f s))

-- | Gets specific component of the state, using a projection function supplied.
gets :: forall tag s m a . MonadState tag s m => (s -> a) -> m a
gets f = fmap f (get @tag)

-- | Encode type-level information for 'StateT'.
data STATE

type instance HandleSuper      STATE s trans   = ()
type instance HandleConstraint STATE s trans m =
  T.MonadState s (trans m)

instance Handle STATE s (T.Strict.StateT s) where
  handling r = r

instance Handle STATE s (T.Lazy.StateT s) where
  handling r = r

instance
    ( Handle STATE s trans
    , Monad m, Monad (trans m)
    ) => MonadState tag s (TaggedTrans (TAGGED STATE tag) trans m)
  where

    get =
      handling @STATE @s @trans @m $
      coerce (T.get @s @(trans m))

    put =
      handling @STATE @s @trans @m $
      coerce (T.put @s @(trans m))

    state =
      handling @STATE @s @trans @m $
      coerce (T.state @s @(trans m) @a) ::
        forall eff a . (s -> (a, s)) -> TaggedTrans eff trans m a

instance
    ( HasLens tag payload s
    , Handle STATE payload trans
    , Monad m, Monad (trans m)
    ) => MonadState tag s (TaggedTrans (TAGGED STATE tag ': effs) trans m)
  where

    get =
      handling @STATE @payload @trans @m $
      (coerce :: forall eff a .
                    trans m a ->
        TaggedTrans eff trans m a)
      (T.gets (view (lensOf @tag @payload @s)))

    put s =
      handling @STATE @payload @trans @m $
      (coerce :: forall eff a .
                    trans m a ->
        TaggedTrans eff trans m a)
      (T.modify (over (lensOf @tag @payload @s) (const s)))

    state f =
      handling @STATE @payload @trans @m $
      (coerce :: forall eff a .
                    trans m a ->
        TaggedTrans eff trans m a)
      (T.state (lensOf @tag @payload @s f))

-- | The parametrizable state monad.
--
-- Computations have access to a mutable state.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of the second.
type State tag r = StateT tag r Identity

-- | The state monad transformer.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of the second.
type StateT tag s = TaggedTrans (TAGGED STATE tag) (T.Strict.StateT s)

-- | Constructor for computations in the state monad transformer.
stateT :: forall tag s m a . (s -> m (a, s)) -> StateT tag s m a
stateT = coerce (T.Strict.StateT @s @m @a)

-- | Runs a 'StateT' with the given initial state
-- and returns both the final value and the final state.
runStateT :: forall tag s m a . StateT tag s m a -> s -> m (a, s)
runStateT = coerce (T.Strict.runStateT @s @m @a)

-- | Runs a 'StateT' with the given initial state
-- and returns the final value, discarding the final state.
evalStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m a
evalStateT = coerce (T.Strict.evalStateT @m @s @a)

-- | Runs a 'StateT' with the given initial state
-- and returns the final state, discarding the final value.
execStateT :: forall tag s m a . Monad m => StateT tag s m a -> s -> m s
execStateT = coerce (T.Strict.execStateT @m @s @a)

-- | Runs a 'State' with the given initial state
-- and returns both the final value and the final state.
runState :: forall tag s a . State tag s a -> s -> (a, s)
runState = coerce (T.Strict.runState @s @a)

-- | Runs a 'State' with the given initial state
-- and returns the final value, discarding the final state.
evalState :: forall tag s a . State tag s a -> s -> a
evalState = coerce (T.Strict.evalState @s @a)

-- | Runs a 'State' with the given initial state
-- and returns the final state, discarding the final value.
execState :: forall tag s a . State tag s a -> s -> s
execState = coerce (T.Strict.execState @s @a)

-- | The parametrizable state monad.
--
-- Computations have access to a mutable state.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of the second.
type LazyState tag r = LazyStateT tag r Identity

-- | The state monad transformer.
--
-- The 'return' function leaves the state unchanged, while '>>=' uses
-- the final state of the first computation as the initial state of the second.
type LazyStateT tag s = TaggedTrans (TAGGED STATE tag) (T.Lazy.StateT s)

-- | Constructor for computations in the state monad transformer.
lazyStateT :: forall tag s m a . (s -> m (a, s)) -> LazyStateT tag s m a
lazyStateT = coerce (T.Lazy.StateT @s @m @a)

-- | Runs a 'StateT' with the given initial state
-- and returns both the final value and the final state.
runLazyStateT :: forall tag s m a . LazyStateT tag s m a -> s -> m (a, s)
runLazyStateT = coerce (T.Lazy.runStateT @s @m @a)

-- | Runs a 'StateT' with the given initial state
-- and returns the final value, discarding the final state.
evalLazyStateT :: forall tag s m a . Monad m => LazyStateT tag s m a -> s -> m a
evalLazyStateT = coerce (T.Lazy.evalStateT @m @s @a)

-- | Runs a 'StateT' with the given initial state
-- and returns the final state, discarding the final value.
execLazyStateT :: forall tag s m a . Monad m => LazyStateT tag s m a -> s -> m s
execLazyStateT = coerce (T.Lazy.execStateT @m @s @a)

-- | Runs a 'State' with the given initial state
-- and returns both the final value and the final state.
runLazyState :: forall tag s a . LazyState tag s a -> s -> (a, s)
runLazyState = coerce (T.Lazy.runState @s @a)

-- | Runs a 'State' with the given initial state
-- and returns the final value, discarding the final state.
evalLazyState :: forall tag s a . LazyState tag s a -> s -> a
evalLazyState = coerce (T.Lazy.evalState @s @a)

-- | Runs a 'State' with the given initial state
-- and returns the final state, discarding the final value.
execLazyState :: forall tag s a . LazyState tag s a -> s -> s
execLazyState = coerce (T.Lazy.execState @s @a)

type family STATES (ts :: HList xs) :: [Type] where
  STATES 'HNil = '[]
  STATES ('HCons t ts) = TAGGED STATE t ': STATES ts

type StatesT s = TaggedTrans (STATES (Tags s)) (T.Strict.StateT s)

type States s = StatesT s Identity

runStatesT :: forall p m a . StatesT p m a -> p -> m (a, p)
runStatesT = coerce (T.Strict.runStateT @p @m @a)

runStates :: forall p a . States p a -> p -> (a, p)
runStates = coerce (T.Strict.runState @p @a)

type StateT' s = StateT s s

stateT' :: (s -> m (a, s)) -> StateT' s m a
stateT' = stateT

runStateT' :: StateT' s m a -> s -> m (a, s)
runStateT' = runStateT

runState' :: State' s a -> s -> (a, s)
runState' = runState

evalStateT' :: Monad m => StateT' s m a -> s -> m a
evalStateT' = evalStateT

type State' s = State s s

evalState' :: State' s a -> s -> a
evalState' = evalState

execStateT' :: Monad m => StateT' s m a -> s -> m s
execStateT' = execStateT

execState' :: State' s a -> s -> s
execState' = execState

type LazyStateT' s = LazyStateT s s

lazyStateT' :: (s -> m (a, s)) -> LazyStateT' s m a
lazyStateT' = lazyStateT

runLazyStateT' :: LazyStateT' s m a -> s -> m (a, s)
runLazyStateT' = runLazyStateT

runLazyState' :: LazyState' s a -> s -> (a, s)
runLazyState' = runLazyState

evalLazyStateT' :: Monad m => LazyStateT' s m a -> s -> m a
evalLazyStateT' = evalLazyStateT

type LazyState' s = LazyState s s

evalLazyState' :: LazyState' s a -> s -> a
evalLazyState' = evalLazyState

execLazyStateT' :: Monad m => LazyStateT' s m a -> s -> m s
execLazyStateT' = execLazyStateT

execLazyState' :: LazyState' s a -> s -> s
execLazyState' = execLazyState

type MonadState' s = MonadState s s

get' :: forall s m . MonadState' s m => m s
get' = get @s

gets' :: forall s m a . MonadState' s m => (s -> a) -> m a
gets' = gets @s

put' :: forall s m . MonadState' s m => s -> m ()
put' = put @s

state' :: forall s m a . MonadState' s m => (s -> (a, s)) -> m a
state' = state @s

modify' :: forall s m . MonadState' s m => (s -> s) -> m ()
modify' = modify @s

-- | Encode type-level information for 'zoom'.
data ZOOM t z

type ZoomT t (z :: Type) = TaggedTrans (ZOOM t z) IdentityT

-- | Zoom into a part of a state using a lens.
zoom
  :: forall tag sOuter sInner m a
   . Lens' sOuter sInner
  -> (forall z . Reifies z (ReifiedLens' sOuter sInner) => ZoomT tag z m a)
  -> m a
zoom l m = reify (Lens l) (\(_ :: Proxy z) -> coerce (m @z))

instance
    ( MonadState tag sOuter m
    , Reifies z (ReifiedLens' sOuter sInner)
    , trans ~ IdentityT
    ) => MonadState tag sInner (TaggedTrans (ZOOM tag z) trans m)
  where
    state =
      (coerce :: forall eff r a .
        (r ->                       m a) ->
        (r -> TaggedTrans eff trans m a))
      (state @tag . l)
      where
        Lens l = reflect (Proxy :: Proxy z)
