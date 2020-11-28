-- | The Csound file
module Csound.Dynamic.Types.CsdFile(
    Csd(..), Flags, Orc(..), Sco(..), Plugin(..), Instr(..), InstrBody,
    CsdEvent, csdEventStart, csdEventDur, csdEventContent, csdEventTotalDur,
    intInstr, alwaysOn
) where

import Csound.Dynamic.Build.Numeric
import Csound.Dynamic.Types.Exp
import Csound.Dynamic.Types.Flags

data Csd = Csd
    { csdFlags   :: Flags
    , csdOrc     :: Orc
    , csdSco     :: Sco
    , csdPlugins :: [Plugin]
    }

data Orc = Orc
    { orcHead           :: InstrBody
    , orcInstruments    :: [Instr]
    }

type InstrBody = E

data Instr = Instr
    { instrName :: InstrId
    , instrBody :: InstrBody
    }

data Sco = Sco
    { scoTotalDur   :: Maybe Double
    , scoGens       :: [(Int, Gen)]
    , scoNotes      :: [(InstrId, [CsdEvent])]  }

data Plugin = Plugin
    { pluginName    :: String
    , pluginContent :: String
    }

----------------------------------------------------------------
-- instruments

intInstr :: Int -> E -> Instr
intInstr n expr = Instr (intInstrId n) expr

----------------------------------------------------------------
-- score

alwaysOn :: InstrId -> (InstrId, [CsdEvent])
alwaysOn instrId = (instrId, [(0, -1, [])])


-- | The Csound note. It's a triple of
--
-- > (startTime, duration, parameters)
type CsdEvent = (Double, Double, Note)

csdEventStart   :: CsdEvent -> Double
csdEventDur     :: CsdEvent -> Double
csdEventContent :: CsdEvent -> Note

csdEventStart   (a, _, _) = a
csdEventDur     (_, a, _) = a
csdEventContent (_, _, a) = a

csdEventTotalDur :: CsdEvent -> Double
csdEventTotalDur (start, dur, _) = start + dur
