module Csound.Render(
    renderCsd, renderCsdBy
) where

import Data.Default
import Data.Maybe(catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Trans.State(evalState)
import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper hiding (double, int)
import Csound.Render.Sco
import Csound.Render.Instr
import Csound.Render.Options
import Csound.Exp.Numeric

import Csound.Render.Pretty

import Csound.Opcode(clip, zeroDbfs)

-- | Renders Csound file.
renderCsd :: [SigOut] -> String
renderCsd = renderCsdBy def

-- | Renders Csound file with options.
renderCsdBy :: CsdOptions -> [SigOut] -> String
renderCsdBy opt as = show $ ppCsdFile 
    (renderFlags opt)
    (renderInstr0 (nchnls lastInstr) (midiAssignTable ids as) opt)
    (ppOrc $ zipWith (renderInstr krateSet) allIds (fmap (substInstrTabs fts) allInstrs))
    (ppSco $ firstInstrNote : lastInstrNote : zipWith (renderScores strs) ids (fmap (substScoreTabs fts) $ scos))
    (renderStringTable strs)
    (renderTotalDur $$ renderTabs fts)
    where scos   = fmap (scoSigOut' . sigOutContent) as          
          (instrs, effects, initOuts) = unzip3 $ zipWith runExpReader as ids    
          fts    = tabMap allInstrs scos
          strs   = stringMap $ concat scos
          ids    = take nInstr [2 .. ]
          
          allInstrs = fmap (defineInstrTabs (tabResolution opt)) $ firstInstr : lastInstr : instrs
          allIds    = firstInstrId : lastInstrId : ids
          
          nInstr = length as
          firstInstrId = 1
          lastInstrId  = nInstr + 2          
        
          firstInstr = execSE $ sequence_ initOuts
          lastInstr  = mixingInstrExp globalEffect effects
           
          scoSigOut' x = case x of
              PlainSigOut _ _ -> defineScoreTabs (tabResolution opt) $ scoSigOut x
              _ -> []            

          dur = maybe 64000000 id $ totalDur as
          renderTotalDur = ppTotalDur dur
          firstInstrNote = alwayson firstInstrId dur
          lastInstrNote  = alwayson lastInstrId dur
          alwayson instrId time = ppNote instrId 0 time []
          krateSet = S.fromList $ csdKrate opt
          globalEffect = csdEffect opt


midiAssignTable :: [Int] -> [SigOut] -> [MidiAssign]
midiAssignTable ids instrs = catMaybes $ zipWith mk ids instrs
    where mk n instr = case sigOutContent $ instr of
            Midi ty chn _ -> Just $ MidiAssign ty chn n
            _ -> Nothing

renderTabs = ppMapTable ppTabDef
renderStringTable = ppMapTable ppStrset

mixingInstrExp :: ([[Sig]] -> SE [Sig]) -> [SE [Sig]] -> E
mixingInstrExp globalEffect effects = execSE $ outs' . fmap clip' =<< globalEffect =<< sequence effects
    where clip' x = clip x 0 zeroDbfs
          
totalDur :: [SigOut] -> Maybe Double
totalDur as 
    | null as'  = Nothing
    | otherwise = Just $ maximum $ map eventEnd . scoSigOut =<< as' 
    where as' = filter isNotMidi $ map sigOutContent as
          isNotMidi x = case x of
            Midi _ _ _ -> False
            _ -> True
  



