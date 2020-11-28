module Csound.Typed.Types.MonoArg(
    MonoArg(..), MonoAdsr, adsrMonoSynt, monoAdsr
) where

import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.Plugins.Adsr140

-- | Input argument for monophonic synthesizer.
-- It includes signals for amplitude, frequency (Cycles Per second), gate, trigger.
-- The gate equals to 1 when any note is pressed or zero when nothing is pressed.
-- The trigger equals to 1 at the moment when new note is pressed otherwise it's 0. 
data MonoArg = MonoArg
    { monoAmp  :: Sig
    , monoCps  :: Sig
    , monoGate :: Sig
    , monoTrig :: Sig }

instance Tuple MonoArg where
    tupleMethods = makeTupleMethods to from
        where
            to :: Sig4 -> MonoArg
            to (amp, cps, gate, trig) = MonoArg amp cps gate trig

            from :: MonoArg -> Sig4
            from (MonoArg amp cps gate trig) = (amp, cps, gate, trig)

-- | ADSR that's used in monophonic instruments.
type MonoAdsr = Sig -> Sig -> Sig -> Sig -> Sig

-- | Turns the function that expects ADSR-function and amplitude and frequency to the
-- function on monophonic argument.
adsrMonoSynt :: (MonoAdsr -> (Sig, Sig) -> a) -> (MonoArg -> a)
adsrMonoSynt f arg = f env (monoAmp arg, monoCps arg) 
    where env = monoAdsr arg

monoAdsr :: MonoArg -> MonoAdsr
monoAdsr arg = adsr140 (monoGate arg) (monoTrig arg) 