module Csound.Exp.Options where

import Data.Default
import Csound.Exp(InstrId)
import Csound.Tab(TabFi, fineFi, idLins, idExps, idConsts, idSplines, idStartEnds)

type CtrlId = Int
type Channel = Int

-- | Csound options. The default value is
--
-- > instance Default CsdOptions where
-- >     def = CsdOptions 
-- >             { flags = "-d"           -- suppress ftable printing
-- >             , sampleRate   = 44100
-- >             , blockSize    = 64
-- >             , setSeed      = Nothing
-- >             , initc7       = []
-- >             , tabFi        = fineFi 13 [(idLins, 11), (idExps, 11), (idConsts, 9), (idSplines, 11), (idStartEndsm 12)] } -- all tables have 8192 points but tables for linear, exponential and constant segments. 

data CsdOptions = CsdOptions 
    { flags         :: String       
    , sampleRate    :: Int          
    , blockSize     :: Int          
    , setSeed       :: Maybe Int    
    , initc7        :: [(Channel, CtrlId, Double)]
    , tabFi         :: TabFi
    }

instance Default CsdOptions where
    def = CsdOptions 
            { flags = "-d"
            , sampleRate  = 44100
            , blockSize = 64
            , setSeed = Nothing
            , initc7 = []
            , tabFi = fineFi 13 [(idLins, 11), (idExps, 11), (idConsts, 9), (idSplines, 11), (idStartEnds, 12)] }

data MidiType = Massign | Pgmassign (Maybe Int)

data MidiAssign = MidiAssign 
    { midiAssignType    :: MidiType
    , midiAssignChannel :: Channel
    , midiAssignInstr   :: InstrId }


