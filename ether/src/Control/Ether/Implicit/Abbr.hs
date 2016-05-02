{-# LANGUAGE TypeFamilies #-}

-- | See "Control.Ether.Abbr".

module Control.Ether.Implicit.Abbr (R, W, S, E) where

import Control.Ether.Abbr (ReifyAbbr)
import Control.Monad.Ether.Implicit

-- | Denotes 'MonadReader'.
data R r
type instance ReifyAbbr (R r) m = MonadReader r m

-- | Denotes 'MonadWriter'.
data W w
type instance ReifyAbbr (W w) m = MonadWriter w m

-- | Denotes 'MonadState'.
data S s
type instance ReifyAbbr (S s) m = MonadState s m

-- | Denotes 'MonadExcept'.
data E e
type instance ReifyAbbr (E e) m = MonadExcept e m

