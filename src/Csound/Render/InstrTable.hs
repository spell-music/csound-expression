{-# Language DeriveFunctor, DeriveFoldable #-}
module Csound.Render.InstrTable(
    InstrTab, MixerTab(..), instrTabs,  mixerTabElems
) where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Arrow(second)

import Data.Monoid
import Data.Traversable(traverse)
import Data.Foldable
import Data.Tuple(swap)
import Data.Maybe(fromJust, mapMaybe)

import Temporal.Music.Score(Score)

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE(History(..))
import Csound.Exp.Mix
import Csound.Exp.Tuple(Out(..), outArity)
import qualified Csound.Render.IndexMap as DM
import Csound.Tfm.Tab
import Csound.Tfm.String
import Csound.Render.Channel

type InstrTab = [(InstrId, E)]
    
data MixerTab a = MixerTab 
    { masterInstr :: (InstrId, a)
    , otherInstr  :: [(InstrId, a)] 
    } deriving (Functor)

instance Foldable MixerTab where
    foldMap f = foldMap f . mixerTabElems

mixerTabElems :: MixerTab a -> [a]
mixerTabElems a = snd (masterInstr a) : fmap snd (otherInstr a)

instrTabs :: TabFi -> Score M -> History -> IO (InstrTab, MixerTab MixerExp, TabMap, StringMap)
instrTabs tabFi sco history = do
    mixers <- mixers sco
    let tabSndSrc = tableSoundSources tabFi history
    tabMixer  <- tableMixers tabFi mixers
    let (instrs', mixers', tabs, strs) = substTablesAndStrings tabSndSrc tabMixer
    return (instrs', mixers', tabs, strs)
            
tableSoundSources :: TabFi -> History -> InstrTab
tableSoundSources tabFi = fmap (second $ defineInstrTabs tabFi) . instrMap
        
tableMixers :: TabFi -> MixerTab Mixer -> IO (MixerTab MixerExp)
tableMixers tabFi = fmap (fmap (defMixTab tabFi)) . mixExps

--------------------------------------------------------------------------
-- define table sizes

defMixTab :: TabFi -> MixerExp -> MixerExp
defMixTab tabFi (MixerExp eff sco) = MixerExp (defineInstrTabs tabFi eff) (fmap defNoteTab sco) 
    where defNoteTab x = case x of
              SoundNote n sco -> SoundNote n $ fmap (defineNoteTabs tabFi) sco
              _ -> x

-- substitute tables 

substTablesAndStrings :: InstrTab -> MixerTab MixerExp -> (InstrTab, MixerTab MixerExp, TabMap, StringMap)
substTablesAndStrings instrTab mixerTab = 
    (fmap (second $ substInstrTabs tabs) instrTab, 
     fmap (substMixFtables strs tabs) mixerTab,
     tabs, 
     strs)
    where notes = getNotes mixerTab
          strs  = stringMap notes
          tabs  = tabMap (getEs instrTab mixerTab) notes

-- collect tables 

getEs :: InstrTab -> MixerTab MixerExp -> [E]
getEs instrTab mixerTab = fmap snd instrTab ++ (fmap mixerExpE $ mixerTabElems mixerTab)

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

mixExps :: MixerTab Mixer -> IO (MixerTab MixerExp)
mixExps (MixerTab master other) = do
    master' <- inPair masterExp master
    other'  <- mapM (inPair mixerExp) other
    return $ MixerTab master' other'
    where inPair f (a, Mixer b sco) = fmap (\x -> (a, MixerExp x sco)) (f b)
            
--------------------------------------------------------------------------
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

mixers :: Score M -> IO (MixerTab Mixer)
mixers sco = undefined
{-
    fmap formRes $ runStateT (traverseMix onSco onMid onMix sco) (initMixingState $ pred lastInstrId)
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
-}

numOfInstrSco :: Score M -> Int
numOfInstrSco as = getSum $ foldMap (Sum . numOfInstrForMix) as

numOfInstrForMix :: M -> Int
numOfInstrForMix x = case x of
    Eff _ a -> 1 + numOfInstrSco a
    _ -> 0

