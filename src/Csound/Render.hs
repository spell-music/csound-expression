{-# Language GADTs, DeriveFunctor, DeriveFoldable #-}
module Csound.Render(
    render    
) where

import Control.Arrow(second)
import Data.Default
import Data.Monoid

import Temporal.Music.Score(temp, stretch, dur, Score, Event(..), tmap, delay)
import qualified Temporal.Music.Score as T(render)

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Render.Pretty
import Csound.Render.Instr
import Csound.Render.Options
import Csound.Render.Channel

import Csound.Tfm.String
import Csound.Tfm.Tab

import Csound.Exp.Arg
import Csound.Exp.Tuple
import Csound.Render.InstrTable
import Csound.Exp.Mix

un = undefined

rescale :: Score (Mix a) -> Score (Mix a)
rescale = tmap $ \e -> let factor = (eventDur e / (mixDur $ eventContent e))
                       in  mixStretch factor (eventContent e)
    where mixDur :: Mix a -> Double
          mixDur x = case x of
            Sco _ a -> dur a
            Mix _ _ a -> dur a
            Mid _ -> 1

          mixStretch :: Double -> Mix a -> Mix a
          mixStretch k x = case x of
            Sco a sco -> Sco a $ stretch k sco
            Mix ar a sco -> Mix ar a $ rescale $ stretch k sco
            Mid _ -> x     

getLastInstrId :: MixerTab a -> Int
getLastInstrId = fst . masterInstr

render :: (Out a) => CsdOptions -> Score (Mix a) -> IO String
render opt a' = do
    (sndTab, mixTab, midiParams, tabs, strs) <- instrTabs (tabFi opt) a
    let lastInstrId = getLastInstrId mixTab
        midiInstrs = fmap (\(MidiInstrParams _ instrId ty chn) -> MidiAssign ty chn instrId) midiParams
        resetMidiInstrId = succ lastInstrId
        midiResetInstrNote = if null midiParams then empty else alwayson totalDur resetMidiInstrId
    return $ show $ ppCsdFile 
        -- flags
        (renderFlags opt)
        -- instr 0 
        (renderInstr0 (nchnls a) midiInstrs opt $$ chnUpdateStmt $$ midiInits midiParams) 
        -- orchestra
        (renderSnd sndTab
            $$ renderMix mixTab
            $$ midiReset resetMidiInstrId midiParams)           
        -- scores
        (lastInstrNotes totalDur (masterInstr mixTab) $$ midiResetInstrNote)
        -- strings
        (ppMapTable ppStrset strs)
        -- ftables
        (ppTotalDur (dur a) $$ ppMapTable ppTabDef tabs)
    where a = rescale a'
          totalDur = dur a'
          
alwayson totalDur instrId = ppNote instrId 0 totalDur []      

renderSnd :: InstrTab E -> Doc
renderSnd = ppOrc . fmap (uncurry renderInstr) . unInstrTab
 
renderMix :: MixerTab MixerExp -> Doc
renderMix (MixerTab master other) = (ppOrc . (uncurry renderMaster master : ) . fmap (uncurry render)) other
    where renderMaster instrId (MixerExp exp _) = ppInstr instrId $ renderMasterPort : renderInstrBody exp
          render instrId (MixerExp exp sco) = ppInstr instrId $ (renderPort $$ renderSco ppEvent sco) : renderInstrBody exp          
          renderPort = ppOpc (ppVar chnVar) "FreePort" []           
          renderMasterPort = ppVar chnVar $= int 0

renderSco :: (InstrId -> Event Double [Prim] -> Var -> Doc) -> Score MixerNote -> Doc
renderSco formNote a = ppSco $ renderNote =<< T.render a
    where renderNote e = case eventContent e of
              MixerNote n     -> return $ formNote n (fmap (const []) e) chnVar
              SoundNote n sco -> fmap (\x -> formNote n x chnVar) $ T.render $ delay (eventStart e) sco -- only delay, stretch was done before
              MidiNote _      -> mempty

              
lastInstrNotes :: Double -> (InstrId, MixerExp) -> Doc
lastInstrNotes totalDur (instrId, a) = alwayson totalDur instrId $$ sco
    where sco = renderSco (\n evt var -> ppMasterNote n evt) $ mixerExpSco a
  
