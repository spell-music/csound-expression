module Csound.Render.Options where

import Data.Default
import Text.PrettyPrint

import Csound.Exp.Wrapper(Channel)
import Csound.Render.Sco

type CtrlId = Int

data CsdOptions = CsdOptions 
    { csdFlags      :: String
    , csdRate       :: Int
    , csdBlockSize  :: Int
    , csdSeed       :: Maybe Int
    , csdInitc7     :: [(Channel, CtrlId, Double)] }

instance Default CsdOptions where
    def = CsdOptions 
            { csdFlags = ""
            , csdRate  = 44100
            , csdBlockSize = 64
            , csdSeed = Nothing
            , csdInitc7 = [] }

renderFlags = text . csdFlags

type Nchnls = Int

data MidiAssign = MidiAssign 
    { midiAssignType    :: MidiType
    , midiAssignChannel :: Channel
    , midiAssignInstr   :: Int }

type InstrId = Int

renderInstr0 :: Nchnls -> [MidiAssign] -> CsdOptions -> Doc
renderInstr0 nchnls massignTable opt = vcat [
    stmt "sr"    $ csdRate opt,
    stmt "ksmps" $ csdBlockSize opt,
    stmt "nchnls" nchnls,   
    maybe empty seed $ csdSeed opt,    
    vcat $ map initc7 $ csdInitc7 opt,    
    vcat $ fmap renderMidiAssign massignTable]
    where stmt a b = text a <+> equals <+> int b
          seed n = text "seed" <+> int n
          initc7 (chn, ctl, val) = text "initc7" <+> 
              (hsep $ punctuate comma [int chn, int ctl, double val])
            
          newline = char '\n'
  
renderMidiAssign :: MidiAssign -> Doc
renderMidiAssign a = opcode <+> (int $ midiAssignChannel a) <> comma <+> (int $ midiAssignInstr a) <> auxParams
    where opcode = text $ case midiAssignType a of
              Massign     -> "massign"
              Pgmassign _ -> "pgmassign"
          auxParams = case midiAssignType a of 
              Pgmassign (Just n) -> comma <+> int n
              _ -> empty  



