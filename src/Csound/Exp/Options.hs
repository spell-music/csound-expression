module Csound.Exp.Options where

import Data.Default
import Csound.Tab(TabFi, fineFi, idSegs, idExps, idConsts)

type CtrlId = Int
type Channel = Int

-- | Csound options. The default value is
--
-- > instance Default CsdOptions where
-- >     def = CsdOptions 
-- >             { flags = "-d"           -- suppress ftable printing
-- >             , sampleRate  = 44100
-- >             , blockSize = 64
-- >             , seed = Nothing
-- >             , initc7 = []
-- >             , tabFi = fineFi 13 [(idSegs, 10), (idExps, 10), (idConsts, 8)] } -- all tables have 8192 points but tables for linear, exponential and constant segments. 

data CsdOptions = CsdOptions 
    { flags         :: String       
    , sampleRate    :: Int          
    , blockSize     :: Int          
    , seed          :: Maybe Int    
    , initc7        :: [(Channel, CtrlId, Double)]
    , tabFi         :: TabFi
    }

instance Default CsdOptions where
    def = CsdOptions 
            { flags = "-d"
            , sampleRate  = 44100
            , blockSize = 64
            , seed = Nothing
            , initc7 = []
            , tabFi = fineFi 13 [(idSegs, 10), (idExps, 10), (idConsts, 8)] }

data MidiType = Massign | Pgmassign (Maybe Int)

data MidiAssign = MidiAssign 
    { midiAssignType    :: MidiType
    , midiAssignChannel :: Channel
    , midiAssignInstr   :: Int }


