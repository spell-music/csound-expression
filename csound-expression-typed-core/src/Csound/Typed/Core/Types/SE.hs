module Csound.Typed.Core.Types.SE
  ( SE (..)
  , newInstr
  , setTotalDur
  , renderSE
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

type GE a = DepT Run a

newtype SE a = SE { unSE :: GE a }
  deriving newtype (Functor, Applicative, Monad)

setTotalDur :: Double -> SE a -> SE a
setTotalDur duration (SE act) = SE $ do
  lift $ State.setTotalDur duration
  act

newInstr :: forall a . Arg a => (a -> SE ()) -> SE InstrId
newInstr instr = SE $ lift $ State.localy $ do
  expr <- renderBody instr
  toInstrId <$> State.insertInstr expr
  where
    renderBody :: (a -> SE ()) -> Run E
    renderBody instrBody = Dynamic.execDepT $ unSE $
      instrBody (toTuple $ pure $ take (tupleArity @a) $ Dynamic.pn <$> [3..])

toInstrId :: Dynamic.InstrId -> InstrId
toInstrId = fromE . pure . Dynamic.instrIdE

renderSE :: Options -> SE () -> IO String
renderSE config (SE act) = fmap (Dynamic.renderCsd def) $ State.exec config $ do
  mainInstr <- Dynamic.execDepT act
  instrId <- State.insertInstr mainInstr
  State.insertNote instrId (0, -1, [])
