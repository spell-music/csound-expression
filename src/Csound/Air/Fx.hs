-- | Effects
module Csound.Air.Fx(    
    -- ** Reverbs
    reverbsc1, rever1, rever2, reverTime,
    smallRoom, smallHall, largeHall, magicCave,
    smallRoom2, smallHall2, largeHall2, magicCave2,

    -- ** Delays
    echo, fdelay, fvdelay, fvdelays, funDelays,

    -- ** Distortion
    distortion,

    -- ** Chorus
    chorus,

    -- ** Flanger
    flange,

    -- ** Phase
    phase1, harmPhase, powerPhase
) where

import Csound.Typed
import Csound.Tab(sines4)
import Csound.Typed.Opcode
import Csound.Types(Sig2)

import Csound.Air.Wave(Lfo, unipolar, oscBy)

-- | Mono version of the cool reverberation opcode reverbsc.
--
-- > reverbsc1 asig feedbackLevel cutOffFreq
reverbsc1 :: Sig -> Sig -> Sig -> Sig
reverbsc1 x k co = 0.5 * (a + b)
    where (a, b) = ar2 $ reverbsc x x k co


---------------------------------------------------------------------------
-- Reverbs

-- | Reverb with given time.
reverTime :: Sig -> Sig -> Sig
reverTime dt a =  nreverb a dt 0.3 

-- | Mono reverb (based on reverbsc)
--
-- > rever1 feedback asig
rever1 :: Sig -> Sig -> (Sig, Sig)
rever1 fbk a = reverbsc a a fbk 12000

-- | Mono reverb (based on reverbsc)
--
-- > rever2 feedback asigLeft asigRight
rever2 :: Sig -> Sig2 -> Sig2
rever2 fbk (a1, a2) = (a1 + wa1, a2 + wa2)
    where (wa1, wa2) = reverbsc a1 a2 fbk 12000

-- | Mono reverb for small room.
smallRoom :: Sig -> (Sig, Sig)
smallRoom = rever1 0.6

-- | Mono reverb for small hall.
smallHall :: Sig -> (Sig, Sig)
smallHall = rever1 0.8

-- | Mono reverb for large hall.
largeHall :: Sig -> (Sig, Sig)
largeHall = rever1 0.9

-- | The magic cave reverb (mono).
magicCave :: Sig -> (Sig, Sig)
magicCave = rever1 0.99

-- | Stereo reverb for small room.
smallRoom2 :: Sig2 -> Sig2
smallRoom2 = rever2 0.6

-- | Stereo reverb for small hall.
smallHall2 :: Sig2 -> Sig2
smallHall2 = rever2 0.8

-- | Stereo reverb for large hall.
largeHall2 :: Sig2 -> Sig2
largeHall2 = rever2 0.9

-- | The magic cave reverb (stereo).
magicCave2 :: Sig2 -> Sig2
magicCave2 = rever2 0.99

-- Delays

-- | The simplest delay with feedback. Arguments are: delay length and decay ratio.
--
-- > echo delayLength ratio
echo :: D -> Sig -> Sig -> SE Sig
echo len fb = fdelay len fb 1

-- | Delay with feedback. 
--
-- > fdelay delayLength decayRatio balance
fdelay :: D -> Sig -> Sig -> Sig -> SE Sig
fdelay len = fvdelay len (sig len)

-- | Delay with feedback. 
--
-- > fdelay maxDelayLength delayLength feedback balance
fvdelay :: D -> Sig -> Sig -> Sig -> Sig -> SE Sig
fvdelay len dt fb mx a = do
    _ <- delayr len
    aDel <- deltap3 dt
    delayw $ a + fb * aDel
    return $ a + (aDel * mx)

-- | Multitap delay. Arguments are: max delay length, list of pairs @(delayLength, decayRatio)@,
-- balance of mixed signal with processed signal.
--
-- > fdelay maxDelayLength  delays balance asig
fvdelays :: D -> [(Sig, Sig)] -> Sig -> Sig -> SE Sig
fvdelays len dtArgs mx a = funDelays len (zip dts fs) mx a
    where 
        (dts, fbks) = unzip dtArgs
        fs = map (*) fbks


-- | Generic multitap delay. It's just like @fvdelays@ but instead of constant feedbackLevel 
-- it expects a function for processing a delayed signal on the tap.
--
-- > fdelay maxDelayLength  delays balance asig
funDelays :: D -> [(Sig, Sig -> Sig)] -> Sig -> Sig -> SE Sig
funDelays len dtArgs mx a = do
    _ <- delayr len
    aDels <- mapM deltap3 dts
    delayw $ a + sum (zipWith ($) fs aDels)
    return $ a + mx * sum aDels 
    where (dts, fs) = unzip dtArgs

-- Distortion

-- | Distortion. 
--
-- > distort distLevel asig
distortion :: Sig -> Sig -> Sig
distortion pre asig = distort1 asig pre 0.5 0 0 `withD` 1

-- Chorus

-- | Chorus.
--
-- > chorus depth rate balance asig
chorus :: Sig -> Sig -> Sig -> Sig -> SE Sig
chorus depth rate mx asig = do
    _ <- delayr 1.2
    adelSig <- deltap3 (0.03 * depth * oscBy fn (3 * rate) + 0.01)
    delayw asig
    return $ ntrpol asig adelSig mx
    where fn = sines4 [(0.5, 1, 180, 1)] -- U-shape parabola

-- Flanger

-- | Flanger. Lfo depth ranges in 0 to 1.
--
-- flanger lfo feedback balance asig
flange :: Lfo -> Sig -> Sig -> Sig -> Sig
flange alfo fbk mx asig = ntrpol asig (flanger asig ulfo fbk) mx
    where ulfo = 0.0001 + 0.02 * unipolar alfo

-- Phaser

-- | First order phaser.
phase1 :: Sig -> Lfo -> Sig -> Sig -> Sig -> Sig
phase1 ord alfo fbk mx asig = ntrpol asig (phaser1 asig (20 + unipolar alfo) ord fbk) mx  

-- | Second order phaser. Sweeping gaps in the timbre are placed harmonicaly
harmPhase :: Sig -> Lfo -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
harmPhase ord alfo q sep fbk mx asig = ntrpol asig (phaser2 asig (20 + unipolar alfo) q ord 1 sep fbk) mx

-- | Second order phaser. Sweeping gaps in the timbre are placed by powers of the base frequency.
powerPhase :: Sig -> Lfo -> Sig -> Sig -> Sig -> Sig -> Sig -> Sig
powerPhase ord alfo q sep fbk mx asig = ntrpol asig (phaser2 asig (20 + unipolar alfo) q ord 2 sep fbk) mx




