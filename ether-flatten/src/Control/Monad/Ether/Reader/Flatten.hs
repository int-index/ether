module Control.Monad.Ether.Reader.Flatten
  ( runReader
  , runReaderT
  ) where

import Control.Ether.Optic
import Control.Lens
import Control.Monad.Ether.Handle
import Control.Monad.Ether.Reader (READER)
import qualified Control.Monad.Reader as T
import Control.Monad.Trans.Ether.Handler
import Data.Coerce
import Data.Kind

type family READERS (ts :: HList xs) :: [Type] where
  READERS 'HNil = '[]
  READERS ('HCons t ts) = TAGGED READER t ': READERS ts

type ReaderT r = Handler (READERS (Tags r)) (T.ReaderT r)

type Reader r = ReaderT r Identity

runReaderT :: forall p m a . ReaderT p m a -> p -> m a
runReaderT = coerce (T.runReaderT @p @_ @m @a)

runReader :: forall p a . Reader p a -> p -> a
runReader = coerce (T.runReader @p @a)
