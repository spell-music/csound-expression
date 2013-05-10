module Csound.Render.Options(
    CsdOptions(..), 
    renderInstr0, renderFlags, MidiAssign(..), CtrlId,
    coarseFi, fineFi
) where

import Data.List(transpose)
import Data.Default
import qualified Data.IntMap as IM(fromList, empty)

import Csound.Exp(TabFi(..), MidiType(..))
import Csound.Exp.Wrapper(Channel, Sig)
import Csound.Exp.SE
import Csound.Exp.Tuple(Out)
import Csound.Render.Pretty
import Csound.Tab(idConsts, idSegs, idExps)

type CtrlId = Int

-- | Csound options. The default value is
--
-- > instance Default CsdOptions where
-- >     def = CsdOptions 
-- >             { flags = "-d"           -- suppress ftable printing
-- >             , sampleRate  = 44100
-- >             , blockSize = 64
-- >             , seed = Nothing
-- >             , initc7 = []
-- >             , tabFi = fineFi 13 [(idSegs, 10), (idExps, 10), (idConsts, 8)] } -- all tables have 8192 points but tables for linear, exponential and constant segments. 

data CsdOptions = CsdOptions 
    { flags         :: String       
    , sampleRate    :: Int          
    , blockSize     :: Int          
    , seed          :: Maybe Int    
    , initc7        :: [(Channel, CtrlId, Double)]
    , tabFi         :: TabFi
    }

instance Default CsdOptions where
    def = CsdOptions 
            { flags = "-d"
            , sampleRate  = 44100
            , blockSize = 64
            , seed = Nothing
            , initc7 = []
            , tabFi = fineFi 13 [(idSegs, 10), (idExps, 10), (idConsts, 8)] }


-- | Sets the same table size for all tables. 
--
-- > coarseFi n
--
-- where @n@  is a degree of 2. For example, @n = 10@ sets size to 1024 points for all tables by default.
coarseFi :: Int -> TabFi
coarseFi n = TabFi n IM.empty

-- | Sets different table size for different GEN-routines. 
--
-- > fineFi n ps 
--
-- where 
-- 
-- * @n@ is the default value for table size (size is a @n@ power of 2) for all gen routines that are not listed in the next argument @ps@.
--
-- * @ps@ is a list of pairs @(genRoutineId, tableSizeDegreeOf2)@ that sets the given table size for a 
--   given GEN-routine.
--
-- with this function we can set lower table sizes for tables that are usually used in the envelopes.
fineFi :: Int -> [(Int, Int)] -> TabFi
fineFi n xs = TabFi n (IM.fromList xs)

renderFlags = text . flags

type Nchnls = Int

type InstrId = Int

renderInstr0 :: Nchnls -> [MidiAssign] -> CsdOptions -> Doc
renderInstr0 nchnls massignTable opt = ppInstr0 $ [
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



