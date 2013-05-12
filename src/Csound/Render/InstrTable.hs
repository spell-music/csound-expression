module Csound.Render.InstrTable(
--    InstrTab, MixerTab(..), instrTabs  
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
import Csound.Exp.GE(History(..))
import Csound.Exp.Mix
import Csound.Exp.Tuple(Out(..), outArity)
import qualified Csound.Render.IndexMap as DM
import Csound.Tfm.Tab
import Csound.Tfm.String
import Csound.Render.Channel
{-
type InstrTab = [(InstrId, E)]
    
data MixerTab = MixerTab 
    { masterInstr :: (InstrId, MixerExp)
    , otherInstr  :: [(InstrId, MixerExp)] } 

mapMixerTab :: (MixerExp -> MixerExp) -> MixerTab -> MixerTab
mapMixerTab f (MixerTab master other) = MixerTab (second f master) (fmap (second f) other)

mixerTabElems :: MixerTab -> [MixerExp]
mixerTabElems (MixerTab master other) = snd master : fmap snd other

instrTabs :: TabFi -> Score M -> History -> IO (InstrTab, MixerTab, TabMap, StringMap)
instrTabs tabFi sco history = fmap (substTablesAndStrings tabSndSrc) $ tabMixer
    where tabSndSrc = tableSoundSources tabFi history
          tabMixer  = tableMixers tabFi sco
            
tableSoundSources :: TabFi -> History -> InstrTab
tableSoundSources tabFi = fmap (second $ defineInstrTabs tabFi) . instrMap
        
--------------------------------------------------------------------------
-- define table sizes

defMixTab :: TabFi -> MixerExp -> MixerExp
defMixTab tabFi (MixerExp eff sco) = MixerExp (defineInstrTabs tabFi eff) (fmap defNoteTab sco) 
    where defNoteTab x = case x of
              SoundNote n sco -> SoundNote n $ fmap (defineNoteTabs tabFi) sco
              _ -> x

-- substitute tables 

substTablesAndStrings :: InstrTab -> MixerTab -> (InstrTab, MixerTab, TabMap, StringMap)
substTablesAndStrings instrTab mixerTab = 
    (fmap (second $ substInstrTabs tabs) instrTab, 
     mapMixerTab (substMixFtables strs tabs) mixerTab,
     tabs, 
     strs)
    where notes = getNotes mixerTab
          strs  = stringMap notes
          tabs  = tabMap (getEs instrTab mixerTab) notes

-- collect tables 

getEs :: InstrTab -> MixerTab -> [E]
getEs instrTab mixerTab = fmap snd instrTab ++ (fmap mixerExpE $ mixerTabElems mixerTab)
    where 
    

getNotes :: MixerTab -> Note
getNotes = foldMap (foldMap scoNotes . mixerExpSco) . mixerTabElems
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

--------------------------------------------------------------------------
-- Gets the mixing instruments.

tableMixers :: TabFi -> Score M -> IO MixerTab
tableMixers tabFi sco = undefined

numOfInstrSco :: Score M -> Int
numOfInstrSco as = getSum $ foldMap (Sum . numOfInstrForMix) as

numOfInstrForMix :: M -> Int
numOfInstrForMix x = case x of
    Eff _ a -> 1 + numOfInstrSco a
    _ -> 0

-}
