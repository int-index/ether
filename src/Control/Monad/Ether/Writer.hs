
-- | See "Control.Monad.Writer".

module Control.Monad.Ether.Writer
    (
    -- * MonadWriter class
      MonadWriter(..)
    , listens
    , censor
    -- * The Writer monad
    , Writer
    , runWriter
    , execWriter
    -- * The WriterT monad transformer
    , WriterT
    , writerT
    , runWriterT
    , execWriterT
    , mapWriterT
    ) where

import Control.Monad.Ether.Writer.Class
import Control.Monad.Trans.Ether.Writer hiding (writer, tell, listen, pass)
