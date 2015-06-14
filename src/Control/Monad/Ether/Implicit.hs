
-- | This module provides convenience exports of all
-- implicitly tagged monad classes from Ether.

module Control.Monad.Ether.Implicit
    ( module Control.Monad
    , module Control.Monad.Fix
    , module Control.Monad.Ether.Reader.Implicit
    , module Control.Monad.Ether.Writer.Implicit
    , module Control.Monad.Ether.State.Implicit
    , module Control.Monad.Ether.Except.Implicit
    ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Ether.Reader.Implicit
import Control.Monad.Ether.Writer.Implicit
import Control.Monad.Ether.State.Implicit
import Control.Monad.Ether.Except.Implicit
