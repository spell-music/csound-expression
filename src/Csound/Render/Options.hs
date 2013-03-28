module Csound.Render.Options(
    CsdOptions(..), mixing, mixingBy,
    renderInstr0, renderFlags, MidiAssign(..), CtrlId
) where

import Data.List(transpose)
import Data.Default

import Csound.Exp.Wrapper(Channel, Sig, SE, Out, Outs)
import Csound.Render.Sco
import Csound.Render.Pretty

type CtrlId = Int

-- | Sums signals for every channel.
mixing :: [[Sig]] -> Outs
mixing = return . fmap sum . transpose

-- | Sums signals for every channel and the processes the output with the given function.
mixingBy :: ([Sig] -> Outs) -> ([[Sig]] -> Outs)
mixingBy f = (f =<<) . mixing 

-- | Csound options. The default value is
--
-- > instance Default CsdOptions where
-- >     def = CsdOptions 
-- >             { csdFlags = ""
-- >             , csdRate  = 44100
-- >             , csdBlockSize = 64
-- >             , csdSeed = Nothing
-- >             , csdInitc7 = []
-- >             , csdEffect = mixing
-- >             , csdKrate  = ["linseg", "expseg", "linsegr", "expsegr", "linen", "linenr", "envlpx"],
-- >             , tabResolution = 8192 }  -- should be power of 2

data CsdOptions = CsdOptions 
    { csdFlags      :: String       
    , csdRate       :: Int          
    , csdBlockSize  :: Int          
    , csdSeed       :: Maybe Int    
    , csdInitc7     :: [(Channel, CtrlId, Double)]
    , csdEffect     :: [[Sig]] -> Outs
    , csdKrate      :: [String]
    , tabResolution :: Int
    }

instance Default CsdOptions where
    def = CsdOptions 
            { csdFlags = ""
            , csdRate  = 44100
            , csdBlockSize = 64
            , csdSeed = Nothing
            , csdInitc7 = []
            , csdEffect = mixing
            , csdKrate  = ["linseg", "expseg", "linsegr", "expsegr", "linen", "linenr", "envlpx"]
            , tabResolution = 8192 }

renderFlags = text . csdFlags

type Nchnls = Int

data MidiAssign = MidiAssign 
    { midiAssignType    :: MidiType
    , midiAssignChannel :: Channel
    , midiAssignInstr   :: Int }

type InstrId = Int

renderInstr0 :: Nchnls -> [MidiAssign] -> CsdOptions -> Doc
renderInstr0 nchnls massignTable opt = ppInstr0 $ [
    stmt "sr"    $ csdRate opt,
    stmt "ksmps" $ csdBlockSize opt,
    stmt "nchnls" nchnls,   
    stmt "0dbfs" 1,
    maybe empty seed $ csdSeed opt] 
    ++ map initc7 (csdInitc7 opt)
    ++ fmap renderMidiAssign massignTable        
    where stmt a b = text a $= int b
          seed n = ppProc "seed" [int n]
          initc7 (chn, ctl, val) = ppProc "initc7" [int chn, int ctl, double val]
            
  
renderMidiAssign :: MidiAssign -> Doc
renderMidiAssign a = ppProc opcode $ [int $ midiAssignChannel a, int $ midiAssignInstr a] ++ auxParams
    where opcode = case midiAssignType a of
              Massign     -> "massign"
              Pgmassign _ -> "pgmassign"
          auxParams = case midiAssignType a of 
              Pgmassign (Just n) -> [int n]
              _ -> []  



