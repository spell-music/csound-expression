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
import Csound.Exp.GE

render :: (Out a) => CsdOptions -> Score (Mix a) -> IO String
render opt a' = undefined 
{-
do
    (ms, history) <- runGE (traverse unMix a') opt
    let a = rescale ms
        tabs = tabMap history
        strs = strMap history
        scos = socres ms history
        
    (sndTab, mixTab, tabs, strs) <- instrTabs (tabFi opt) a history
    let lastInstrId = getLastInstrId mixTab
    return $ show $ ppCsdFile 
        -- flags
        (renderFlags opt)
        -- instr 0 
        (renderInstr0 (nchnls a') (midiInstrs history) opt $$ chnUpdateStmt) 
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

strMap :: History -> StringMap
strMap = undefined

tabMap :: History -> TabMap
tabMap = undefined

scores :: Score M -> History -> Doc
scores = undefined

instrs :: History -> [(InstrId, Doc)]
instrs = undefined

alwayson totalDur instrId = ppNote instrId 0 totalDur []      

renderSnd :: InstrTab -> Doc
renderSnd = ppOrc . fmap (uncurry renderInstr)
 
renderMix :: MixerTab -> Doc
renderMix (MixerTab master other) = (ppOrc . (uncurry renderMaster master : ) . fmap (uncurry render)) other
    where renderMaster instrId (MixerExp exp _) = ppInstr instrId $ renderMasterPort : renderInstrBody exp
          render instrId (MixerExp exp sco) = ppInstr instrId $ (renderPort $$ renderSco ppEvent sco) : renderInstrBody exp          
          renderPort = ppOpc (ppVar chnVar) chnUpdateOpcodeName []           
          renderMasterPort = ppVar chnVar $= int 0

renderSco :: (InstrId -> Event Double [Prim] -> Var -> Doc) -> Score MixerNote -> Doc
renderSco formNote a = ppSco $ renderNote =<< T.render a
    where renderNote e = case eventContent e of
              MixerNote n     -> return $ formNote n (fmap (const []) e) chnVar
              SoundNote n sco -> fmap (\x -> formNote n x chnVar) $ T.render $ delay (eventStart e) sco -- only delay, stretch was done before
              
lastInstrNotes :: Double -> (InstrId, MixerExp) -> Doc
lastInstrNotes totalDur (instrId, a) = alwayson totalDur instrId $$ sco
    where sco = renderSco (\n evt var -> ppMasterNote n evt) $ mixerExpSco a
  
getLastInstrId :: MixerTab -> InstrId
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
-}
