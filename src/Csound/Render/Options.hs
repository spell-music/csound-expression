module Csound.Render.Options(
    renderInstr0, renderFlags
) where

import Csound.Exp.Options
import Csound.Render.Pretty
import Csound.Render.Channel(chnUpdateStmt)

renderFlags :: CsdOptions -> Doc
renderFlags = text . flags

type Nchnls = Int

renderInstr0 :: Nchnls -> [MidiAssign] -> CsdOptions -> Doc
renderInstr0 nchnls massignTable opt = ($$ chnUpdateStmt) $ ppInstr0 $ [
    stmt "sr"    $ sampleRate opt,
    stmt "ksmps" $ blockSize opt,
    stmt "nchnls" nchnls,   
    stmt "0dbfs" 1,
    maybe empty stmtSeed $ seed opt] 
    ++ map stmtInitc7 (initc7 opt)
    ++ fmap renderMidiAssign massignTable        
    where stmt a b = text a $= int b
          stmtSeed n = ppProc "seed" [int n]
          stmtInitc7 (chn, ctl, val) = ppProc "initc7" [int chn, int ctl, double val]
            
  
renderMidiAssign :: MidiAssign -> Doc
renderMidiAssign a = ppProc opcode $ [int $ midiAssignChannel a, int $ midiAssignInstr a] ++ auxParams
    where opcode = case midiAssignType a of
              Massign     -> "massign"
              Pgmassign _ -> "pgmassign"
          auxParams = case midiAssignType a of 
              Pgmassign (Just n) -> [int n]
              _ -> []  



