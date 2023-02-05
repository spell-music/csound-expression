module Csound.Typed.Core.Types.SE
  ( SE (..)
  , newInstr
  , setTotalDur
  , renderSE
  , global
  ) where

import Csound.Dynamic (DepT, E)
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.State.Options (Options)
import Csound.Typed.Core.State qualified as State
import Csound.Typed.Core.Types.Prim
import Csound.Typed.Core.Types.Tuple
import Control.Monad.Trans.Class (lift)
import Data.Default

type Dep a = DepT Run a

newtype SE a = SE { unSE :: Dep a }
  deriving newtype (Functor, Applicative, Monad)

setTotalDur :: Double -> SE a -> SE a
setTotalDur duration (SE act) = SE $ do
  lift $ State.setTotalDur duration
  act

newInstr :: forall a . Arg a => (a -> SE ()) -> SE (InstrId D a)
newInstr instr = SE $ lift $ State.localy $ do
  expr <- renderBody instr
  toInstrId <$> State.insertInstr expr
  where
    renderBody :: (a -> SE ()) -> Run E
    renderBody instrBody = Dynamic.execDepT $ unSE $
      instrBody (toTuple $ pure $ take (tupleArity @a) $ zipWith Dynamic.pn (tupleRates @a) [4..])

    toInstrId = InstrId . fromE . pure . Dynamic.prim . Dynamic.PrimInstrId

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
