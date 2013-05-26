{-# Language TupleSections #-}
module Csound.Render(
    render    
) where

import Data.Traversable
import Control.Monad.Trans.Writer
import qualified Data.IntMap as IM

import Csound.Exp
import Csound.Exp.Instr(effectExp)
import Csound.Exp.SE
import Csound.Exp.Options
import Csound.Render.Pretty
import Csound.Render.Instr
import Csound.Render.Options
import Csound.Render.Channel

import Csound.Exp.Tuple(Out)
import Csound.Exp.Mix
import Csound.Exp.GE
import Csound.Exp.EventList

render :: (Out a, CsdSco f) => CsdOptions -> f (Mix a) -> IO String
render opt sigs = fmap (show . renderHistory (nchnls sigs) (csdEventListDur events) opt) 
    $ flip execGE opt $ do
        notes <- traverse unMix events
        instrId <- saveMixerInstr =<< effectExp (proxy masterOuts sigs)
        let notes' = rescaleCsdEventListM $ toCsdEventList notes 
        saveMixerNotes $ toLowLevelNotesMap $ Eff instrId notes'
        saveAlwaysOnNote instrId
    where events = toCsdEventList sigs
          proxy :: (Out a) => (a -> SE ()) -> f (Mix a) -> (a -> SE ()) 
          proxy = const

toLowLevelNotesMap :: M -> IM.IntMap LowLevelSco
toLowLevelNotesMap mixNotes = IM.fromList $ execWriter $ phi mixNotes
    where    
        phi :: M -> Writer [(Int, LowLevelSco)] ()
        phi x = case x of
            Eff instrId notes -> 
                let (instrNotes, rest) = onEff notes
                in  tell [(instrIdCeil instrId, instrNotes)] >> mapM_ phi rest
            Snd _ _ -> error "Render.hs:toLowLevelNotesMap no effect instrument, end up in Snd case"

onEff :: CsdEventList M -> (LowLevelSco, [M])
onEff (CsdEventList _ events) = execWriter $ mapM_ phi events
    where phi :: CsdEvent M -> Writer (LowLevelSco, [M]) ()
          phi (start, dur, content) = case content of
            Snd instrId notes -> tellFst $ fmap (instrId, ) $ csdEventListNotes $ delayCsdEventList start notes
            Eff instrId _     -> tell ([(instrId, (start, dur, []))], [content])
          tellFst x = tell (x, [])

renderHistory :: Int -> Double -> CsdOptions -> History -> Doc
renderHistory numOfChnls totalDur options history = ppCsdFile 
    -- flags
    (renderFlags options) 
    -- instr 0
    (renderInstr0 numOfChnls (midis history) options)
    -- orchestra
    (renderOrc $ instrs history)
    -- scores
    (renderSco $ scos history)
    -- strings
    (ppMapTable ppStrset $ strIndex history)
    -- ftables
    (ppTotalDur totalDur $$ (ppMapTable ppTabDef $ tabIndex history))    

    
renderSco :: Scos -> Doc
renderSco x = vcat $ fmap ppAlwayson $ alwaysOnInstrs x

renderOrc :: Instrs -> Doc
renderOrc x = (vcatMap renderSource $ instrSources x) $$ (vcatMap renderMixer $ instrMixers x)
    where getMixerNotes instrId = (fmap renderNotes $ mixerNotes x) IM.! (instrIdCeil instrId)
          
          renderSource = uncurry renderInstr    
          renderMixer  (instrId, expr) = ppInstr instrId $
               ppFreeChnStmt
            $$ getMixerNotes instrId
            $$ renderInstrBody expr

renderNotes :: LowLevelSco -> Doc
renderNotes notes = vcat $ fmap (\(instrId, evt) -> ppEvent instrId evt chnVar) notes

