module Csound.Render.Options(
    renderInstr0, renderFlags
) where

import Csound.Exp(InstrId(..))
import Csound.Exp.Options
import Csound.Exp.GE
import Csound.Exp.Gui(Panel, drawGui)
import Csound.Exp.SE(execSE)
import Csound.Render.Pretty
import Csound.Render.Instr(renderInstrBody)
import Csound.Render.Channel(chnUpdateStmt)

renderFlags :: [MidiAssign] -> CsdOptions -> Doc
renderFlags ms = reactOnMidi ms . text . flags
    where 
        reactOnMidi xs    
            | null xs   = id
            | otherwise = (text "-Ma" <+>)
            
type Nchnls = Int

renderInstr0 :: Nchnls -> [MidiAssign] -> [Global] -> [Panel] -> CsdOptions -> Doc
renderInstr0 nchnls massignTable globalVars panels opt = ppInstr0 $ [
    stmt "sr"    $ sampleRate opt,
    stmt "ksmps" $ blockSize opt,
    stmt "nchnls" nchnls,   
    stmt "0dbfs" 1,
    maybe empty stmtSeed $ seed opt,
    renderInstrBody $ execSE $ initGlobals globalVars,
    vcat $ fmap stmtInitc7 (initc7 opt),
    vcat $ fmap renderMidiAssign massignTable,
    chnUpdateStmt,
    guiStmt ]
    where stmt a b = text a $= int b
          stmtSeed n = ppProc "seed" [int n]
          stmtInitc7 (chn, ctl, val) = ppProc "initc7" [int chn, int ctl, double val]
            
          guiStmt 
            | null panels = empty 
            | otherwise   = (vcat $ fmap drawGui panels) $$ text "FLrun"
  
renderMidiAssign :: MidiAssign -> Doc
renderMidiAssign a = ppProc opcode $ [int $ midiAssignChannel a, int $ instrIdCeil $ midiAssignInstr a] ++ auxParams
    where opcode = case midiAssignType a of
              Massign     -> "massign"
              Pgmassign _ -> "pgmassign"
          auxParams = case midiAssignType a of 
              Pgmassign (Just n) -> [int n]
              _ -> []  


