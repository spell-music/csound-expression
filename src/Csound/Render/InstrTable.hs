{-# Language DeriveFunctor, DeriveFoldable #-}
module Csound.Render.InstrTable(
    InstrTab(..), MixerTab(..), instrTabs,
    instrTabElems, mixerTabElems
) where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Arrow(second)

import Data.Monoid
import Data.Foldable
import Data.Tuple(swap)
import Data.Maybe(fromJust, mapMaybe)

import Temporal.Music.Score(Score)

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Mix
import Csound.Exp.Tuple(Out(..), outArity)
import qualified Csound.Render.IndexMap as DM
import Csound.Tfm.Tab
import Csound.Tfm.String
import Csound.Render.Channel

newtype InstrTab a = InstrTab { unInstrTab :: [(InstrId, a)] }
    deriving (Functor, Foldable) 
    
instrTabElems :: InstrTab a -> [a]
instrTabElems (InstrTab as) = fmap snd as
    
data MixerTab a = MixerTab 
    { masterInstr :: (InstrId, a)
    , otherInstr  :: [(InstrId, a)] 
    } deriving (Functor)

instance Foldable MixerTab where
    foldMap f = foldMap f . mixerTabElems

mixerTabElems :: MixerTab a -> [a]
mixerTabElems a = snd (masterInstr a) : fmap snd (otherInstr a)

instrTabs :: Out a => TabFi 
    -> Score (Mix a) -> IO (InstrTab E, MixerTab MixerExp, [MidiInstrParams], TabMap, StringMap)
instrTabs tabFi sco = do
    instrs <- soundSources sco
    mixers <- mixers instrs sco
    let midiParams = getMidiInstrParams instrs
        (instrs', mixers', tabs, strs) = substTablesAndStrings 
            (tableSoundSources tabFi instrs) (tableMixers tabFi mixers)
    return (instrs', mixers', midiParams, tabs, strs)
            
tableSoundSources :: TabFi -> InstrIndexMap -> InstrTab E
tableSoundSources tabFi = InstrTab . fmap phi . DM.elems
    where phi (exp, name) = (name, defineInstrTabs tabFi $ instrExp name exp)
        
tableMixers :: TabFi -> MixerTab Mixer -> MixerTab MixerExp    
tableMixers tabFi = fmap (defMixTab tabFi) . mixExps

--------------------------------------------------------------------------
-- define table sizes

defMixTab :: TabFi -> MixerExp -> MixerExp
defMixTab tabFi (MixerExp eff sco) = MixerExp (defineInstrTabs tabFi eff) (fmap defNoteTab sco) 
    where defNoteTab x = case x of
              SoundNote n sco -> SoundNote n $ fmap (defineNoteTabs tabFi) sco
              _ -> x

-- substitute tables 

substTablesAndStrings :: InstrTab E -> MixerTab MixerExp -> (InstrTab E, MixerTab MixerExp, TabMap, StringMap)
substTablesAndStrings instrTab mixerTab = 
    (fmap (substInstrTabs tabs) instrTab, 
     fmap (substMixFtables strs tabs) mixerTab,
     tabs, 
     strs)
    where notes = getNotes mixerTab
          strs  = stringMap notes
          tabs  = tabMap (getEs instrTab mixerTab) notes

-- collect tables 

getEs :: InstrTab E -> MixerTab MixerExp -> [E]
getEs instrTab mixerTab = 
    instrTabElems instrTab ++ (fmap mixerExpE $ mixerTabElems mixerTab)

getNotes :: MixerTab MixerExp -> Note
getNotes = foldMap (foldMap scoNotes . mixerExpSco)
    where scoNotes :: MixerNote -> Note
          scoNotes x = case x of
            SoundNote n sco -> fold sco
            _ -> mempty    

-- substitute tables for integer identifiers

-- substitute strings
     
substMixFtables :: StringMap -> TabMap -> MixerExp -> MixerExp
substMixFtables strMap tabMap (MixerExp exp sco) = 
    MixerExp (substInstrTabs tabMap exp) (fmap substNote sco)
    where substNote x = case x of
            SoundNote n sco -> SoundNote n $ fmap subst sco
            _ -> x
          subst = substNoteStrs strMap . substNoteTabs tabMap 

-------------------------------------------------------------------------
-- render mixers

mixExps :: MixerTab Mixer -> MixerTab MixerExp
mixExps (MixerTab master other) = 
    MixerTab (second masterMixerExp master) (fmap (second mixerExp) other) 

--------------------------------------------------------------------------
--
-- Gets sound sources from scores. We extract all instruments
-- that lie in the bottom of the score hierarchy. Sound source
-- is an instrument that is trigered with notes or midi messages.
--
-- We just traverse the Score and insert all instruments into the 
-- special container DM.IndexMap. IndexMap is a Set that relies 
-- on stable names (OMG).

-- Instruments that are referenced with stable names.
type InstrIndexMap = DM.IndexMap Instr

type MkIndexMap = State (IO InstrIndexMap) ()
    
insertInstr :: Instr -> MkIndexMap
insertInstr a = modify (DM.insert (instrName a) a =<<)

soundSources :: Score (Mix a) -> IO InstrIndexMap
soundSources sco = execState indexMap (return $ DM.empty 1)
    where indexMap = traverseMix onSco onMid onMix sco >> return ()
            
          onMix _ _ _   = return ()
          onSco instr _ = insertInstr instr
          onMid instr   = insertInstr instr

--------------------------------------------------------------------------
--
-- Gets the mixing instruments.

type MkMixerTab a = StateT MixingState IO a

data MixingState = MixingState 
    { mixingCounter :: Int
    , mixingElems   :: [(Int, Mixer)] }

initMixingState :: Int -> MixingState
initMixingState n = MixingState n []

saveElem :: Int -> Mixer -> MkMixerTab ()
saveElem n a = modify $ \x -> x{ mixingElems = (n, a) : mixingElems x }

getCounter :: MkMixerTab Int
getCounter = fmap mixingCounter get

putCounter :: Int -> MkMixerTab ()
putCounter n = modify $ \s -> s{ mixingCounter = n }

mixers :: Out a => InstrIndexMap -> Score (Mix a) -> IO (MixerTab Mixer)
mixers tab sco = fmap formRes $ runStateT (traverseMix onSco onMid onMix sco) (initMixingState $ pred lastInstrId)
    where formRes (sco, st) = MixerTab (lastInstrId, Mixer (Arity n n) return sco) (mixingElems st)
          lastInstrId = 1 + DM.length tab + numOfInstrSco sco
          n = nchnls sco     

          onSco snd sco = do
                Just n <- lift $ DM.lookup (instrName snd) tab
                return $ SoundNote n sco
          onMid snd = do
                Just n <- lift $ DM.lookup (instrName snd) tab
                return $ MidiNote $ fromJust $ extractInstrMidiParams n snd
          onMix ar eff notes = do
                n <- getCounter
                putCounter $ pred n
                saveElem n $ Mixer ar eff notes
                return $ MixerNote n        


numOfInstrSco :: Score (Mix a) -> Int
numOfInstrSco as = getSum $ foldMap (Sum . numOfInstrForMix) as

numOfInstrForMix :: Mix a -> Int
numOfInstrForMix x = case x of
    Mix _ _ a -> 1 + numOfInstrSco a
    Mid _     -> 0
    Sco _ _   -> 0

----------------------------------------------------------------------
-- Midi parameters

getMidiInstrParams :: InstrIndexMap -> [MidiInstrParams]
getMidiInstrParams a = mapMaybe (uncurry $ flip extractInstrMidiParams) $ DM.elems a

extractInstrMidiParams :: InstrId -> Instr -> Maybe MidiInstrParams
extractInstrMidiParams name x = case instrMidi x of
    Just (midiType, chn) -> Just $ MidiInstrParams (instrArity x) name midiType chn
    _ -> Nothing

