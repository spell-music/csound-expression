module Csound.Core.Types.SE.Type
  ( SE (..)
  ) where

import Csound.Core.State (Dep)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)

newtype SE a = SE { unSE :: Dep a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail SE where
  fail = error "no implementation for MonadFail"

instance MonadIO SE where
  liftIO = SE . lift . liftIO


