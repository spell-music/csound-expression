module Csound.Typed.Core.Types.SE
  ( SE (..)
  , setTotalDur
  , renderSE
  , global
  , IsRef (..)
  , modifyRef
  , getCurrentRate
  ) where

import Control.Monad.IO.Class

import Csound.Dynamic (IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Dep)
import Csound.Typed.Core.State.Options (Options)
import Csound.Typed.Core.State qualified as State
import Csound.Typed.Core.Types.Tuple
import Control.Monad.Trans.Class (lift)
import Data.Default

newtype SE a = SE { unSE :: Dep a }
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO SE where
  liftIO = SE . lift . liftIO

setTotalDur :: Double -> SE a -> SE a
setTotalDur duration (SE act) = SE $ do
  lift $ State.setTotalDur duration
  act

renderSE :: Options -> SE () -> IO String
renderSE config (SE act) = fmap (Dynamic.renderCsd def) $ State.exec config $ do
  mainInstr <- Dynamic.execDepT act
  instrId <- State.insertInstr mainInstr
  State.insertNote instrId (0, -1, [])

-- | Adds expression to the global scope.
-- It is instrument 0 in csound terms.
global :: SE () -> SE ()
global (SE expr) = SE $ lift $ do
  ge <- Dynamic.execDepT expr
  State.insertGlobalExpr ge

class IsRef ref where
  readRef  :: Tuple a => ref a -> SE a
  writeRef :: Tuple a => ref a -> a -> SE ()

  mixRef   :: (Num a, Tuple a) => ref a -> a -> SE ()
  clearRef :: (Num a, Tuple a) => ref a -> SE ()

  mixRef ref a = modifyRef ref (a +)
  clearRef ref = writeRef ref 0

modifyRef :: (Tuple a, IsRef ref) => ref a -> (a -> a) -> SE ()
modifyRef ref f = writeRef ref . f =<< readRef ref

getCurrentRate :: SE (Maybe IfRate)
getCurrentRate = SE $ lift State.getCurrentRate

