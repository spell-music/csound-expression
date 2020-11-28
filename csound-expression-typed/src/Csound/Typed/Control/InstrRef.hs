-- | Imperative csound instruments
module Csound.Typed.Control.InstrRef(
    InstrRef, newInstr, scheduleEvent, turnoff2, negateInstrRef, addFracInstrRef,
    newOutInstr, noteOn, noteOff
) where    

import Control.Monad
import Control.Monad.Trans.Class

import Control.Applicative
import Data.Default
import Csound.Dynamic(InstrId(..), Rate(..), DepT, depT_, opcs)
import qualified Csound.Typed.GlobalState.Elements as C

import Csound.Typed.Types
import Csound.Typed.GlobalState hiding (turnoff2) 
import Csound.Typed.Control.Ref

-- | Fractional part of the instrument dentifier.
data InstrFrac = InstrFrac 
    { instrFracValue :: D
    , instrFracSize  :: D    
    }

-- | Instrument reference. we can invoke or stop the instrument by the identifier.
data InstrRef a = InstrRef 
    { instrRefMain :: D
    , instrRefFrac :: Maybe InstrFrac }

-- | Creates a new instrument and generates a unique identifier.
newInstr ::  (Arg a) => (a -> SE ()) -> SE (InstrRef a)
newInstr instr = geToSe $ fmap fromInstrId $ saveInstr $ instr toArg

-- | Schedules an event for the instrument. 
--
-- > scheduleEvent instrRef delay duration args
--
-- The arguments for time values are set in seconds.
scheduleEvent :: (Arg a) => InstrRef a -> D -> D -> a -> SE ()
scheduleEvent instrRef start end args = SE $ hideGEinDep $ fmap C.event $ C.Event <$> toGE (getInstrId instrRef) <*> toGE start <*> toGE end <*> toNote args

getInstrId :: InstrRef a -> D
getInstrId (InstrRef value frac) = value + maybe 0 fromFrac frac
    where
        fromFrac (InstrFrac value size) = (value * 10 + 1) / (size * 10)

-- | Negates the instrument identifier. This trick is used in Csound to update the instrument arguments while instrument is working.
negateInstrRef :: InstrRef a -> InstrRef a 
negateInstrRef ref = ref { instrRefMain = negate $ instrRefMain ref }

-- | Adds fractional part to the instrument reference. This trick is used in Csound to identify the notes (or specific instrument invokation).
addFracInstrRef :: D -> D -> InstrRef a -> InstrRef a
addFracInstrRef maxSize value instrRef = instrRef { instrRefFrac = Just (InstrFrac value maxSize) }

fromInstrId :: InstrId -> InstrRef a
fromInstrId x = case x of
    InstrId frac ceil -> InstrRef (int ceil) Nothing
    InstrLabel _    -> error "No reference for string instrument id. (Csound.Typed.Control.Instr.hs: fromInstrId)"

-- | Creates an insturment that produces a value.
newOutInstr :: (Arg a, Sigs b) => (a -> SE b) -> SE (InstrRef a, b)
newOutInstr f = do
    ref <- newClearableGlobalRef 0
    instrId <- newInstr $ \a -> mixRef ref =<< f a
    aout <- readRef ref
    return (instrId, aout)

-- | Triggers a note with fractional instrument reference. We can later stop the instrument on specific note with function @noteOff@.
noteOn :: (Arg a) => D -> D -> InstrRef a -> a -> SE ()
noteOn maxSize noteId instrId args = scheduleEvent (addFracInstrRef maxSize noteId instrId) 0 (-1) args

-- | Stops a note with fractional instrument reference.
noteOff :: (Default a, Arg a) => D -> D -> InstrRef a -> SE () 
noteOff maxSize noteId instrId = scheduleEvent (negateInstrRef $ addFracInstrRef maxSize noteId instrId) 0 0.01 def

-- | Turns off the note played on the given instrument.
-- Use fractional instrument reference to turn off specific instance.
--
-- > turnoff2 instrRef mode releaseTime
--
-- The mode is sum of the following values: 
-- 
-- * 0, 1, or 2: turn off all instances (0), oldest only (1), or newest only (2) 
-- 
-- * 4: only turn off notes with exactly matching (fractional) instrument number, rather than ignoring fractional part 
--
-- * 8: only turn off notes with indefinite duration (idur < 0 or MIDI) 
--
-- @releaseTime@  if non-zero, the turned off instances are allowed to release, otherwise are deactivated immediately (possibly resulting in clicks).
turnoff2 :: InstrRef a -> Sig -> Sig -> SE ()
turnoff2 instrRef kmode krelease = go (sig $ getInstrId instrRef) kmode krelease
    where
        go :: Sig -> Sig -> Sig -> SE ()
        go instr mode release = SE $ join $ lift $ csdTurnoff2 <$> (toGE instr) <*> (toGE mode) <*> (toGE release)

        csdTurnoff2 :: Monad m => E -> E -> E -> DepT m ()
        csdTurnoff2 instrId mode release = depT_ $ opcs "turnoff2" [(Xr, [Kr, Kr, Kr])] [instrId, mode, release]