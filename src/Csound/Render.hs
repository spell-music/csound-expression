module Csound.Render(
    render    
) where

import Data.Traversable(traverse)
import Data.Monoid

import Temporal.Music.Score(temp, stretch, dur, Score, Event(..), tmap, delay)
import qualified Temporal.Music.Score as T(render)

import Csound.Exp
import Csound.Render.Pretty
import Csound.Render.Instr
import Csound.Render.Options
import Csound.Render.Channel

import Csound.Exp.Tuple(Out)
import Csound.Render.InstrTable
import Csound.Exp.Mix
import Csound.Exp.SE

render :: (Out a) => CsdOptions -> Score (Mix a) -> IO String
render opt a' = do
    (ms, history) <- runSE $ traverse unMix a'
    midis <- mapM (pureMidiAssign (instrMap history)) (midiInstrs history)
    let a = rescale ms
    (sndTab, mixTab, tabs, strs) <- instrTabs (tabFi opt) a history
    let lastInstrId = getLastInstrId mixTab
    return $ show $ ppCsdFile 
        -- flags
        (renderFlags opt)
        -- instr 0 
        (renderInstr0 (nchnls a') midis opt $$ chnUpdateStmt) 
        -- orchestra
        (renderSnd sndTab
            $$ renderMix mixTab)           
        -- scores
        (lastInstrNotes totalDur (masterInstr mixTab))
        -- strings
        (ppMapTable ppStrset strs)
        -- ftables
        (ppTotalDur (dur a) $$ ppMapTable ppTabDef tabs)
    where totalDur = dur a'


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
              
lastInstrNotes :: Double -> (InstrId, MixerExp) -> Doc
lastInstrNotes totalDur (instrId, a) = alwayson totalDur instrId $$ sco
    where sco = renderSco (\n evt var -> ppMasterNote n evt) $ mixerExpSco a
  
getLastInstrId :: MixerTab a -> Int
getLastInstrId = fst . masterInstr
          
rescale :: Score M -> Score M
rescale = tmap $ \e -> let factor = (eventDur e / (mixDur $ eventContent e))
                       in  mixStretch factor (eventContent e)
    where mixDur :: M -> Double
          mixDur x = case x of
            Snd _ a -> dur a
            Eff _ a -> dur a

          mixStretch :: Double -> M -> M
          mixStretch k x = case x of
            Snd a sco -> Snd a $ stretch k sco
            Eff a sco -> Eff a $ rescale $ stretch k sco
