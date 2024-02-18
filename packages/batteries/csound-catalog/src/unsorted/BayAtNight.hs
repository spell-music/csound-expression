module BayAtNight where

import Control.Monad

import Csound.Base

main = dac $ do 
    asig <- midi 1 instr
    let a1 = reverbN 8 0.98 0.8 20000 $ chorus 2 30 $ asig
    return $ fmap (effect ((k*) . onBass)) a1
    where k = 0.1

bayAtNightEffect :: Sig -> SE (Sig, Sig)
bayAtNightEffect 
    = effect (bassEnhancment 100 1.5) 
    . reverbN 8 0.98 0.8 20000 . chorus 2 30

onBass = bassEnhancment 100 1.5

instr :: Msg -> Sig
instr msg = 0.5 * env * stringPad 10 amp (sig cps) 
    where (amp, cps) = (ampmidi msg, cpsmidi msg)
          env = linsegr [1, 1, 1] 1.5 0

stringPad :: D -> Sig -> Sig
stringPad amp cps = aout
    where
        -- skight chorus effect
        asig = (kctrl * ) $ sum $ fmap (oscBy wave . (cps + )) [0.1, 0, -0.1]
        aout = butlp asig (sig $ (amp - 0.5) * (40 * 127) + 900)

        wave = sines [1, 0.5, 0.33, 0.25, 0, 0.1, 0.1, 0.1]

-- effects

pan :: D -> Sig -> (Sig, Sig)
pan ipos asig = (sig (cos ippan) * asig, sig (sin ippan) * asig)
    where ippan = ipos*1.570796325  -- half of PI (radians of 90o angle)

-- Chorus effect, borrowed from http://www.jlpublishing.com/Csound.htm
-- I made some of its parameters accesible trhough score
-- delay in milliseconds
chorus :: D -> D -> Sig -> Sig
chorus idlym iscale asig = 0.5 * aout
    where 
        phi cps maxDel = vdelay3 asig (sig (idlym / 5) + sig (idlym / iscale) * osc cps) maxDel
        aout = sum $ zipWith phi 
            [1, 0.995, 1.05, 1]
            [900, 700, 700, 900]

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Reverb
-- 8 delay line FDN reverb, with feedback matrix based upon 
-- physical modeling scattering junction of 8 lossless waveguides
-- of equal characteristic impedance. Based on Julius O. Smith III, 
-- "A New Approach to Digital Reverberation using Closed Waveguide
-- Networks," Proceedings of the International Computer Music 
-- Conference 1985, p. 47-53 (also available as a seperate
-- publication from CCRMA), as well as some more recent papers by
-- Smith and others.
--
-- Coded by Sean Costello, October 1999 (in Csound)

{-
  igain = p4      ; gain of reverb. adjust empirically
                  ; for desired reverb time. .6 gives
                  ; a good small "live" room sound, .8
                  ; a small hall, .9 a large hall,
                  ; .99 an enormous stone cavern.
  
  ipitchmod = p5  ; amount of random pitch modulation
                  ; for the delay lines. 1 is the "normal"
                  ; amount, but this may be too high for
                  ; held pitches such as piano tones.
                  ; adjust to taste.
  
  itone = p6      ; cutoff frequency of lowpass filters
                  ; in feedback loops of delay lines,
                  ; in hz. lower cutoff frequencies results
                  ; in a sound with more high-frequency
                  ; damping.
-}
reverbN :: Int -> D -> D -> D -> Sig -> SE (Sig, Sig)
reverbN n igain ipitchmod itone asig = do
    afiltRefs   <- mapM newSERef $ replicate n 0
    afilts      <- mapM readSERef afiltRefs 
    let apj     = (2 / fromIntegral n) * sum afilts
    adels       <- sequence $ zipWith3 (del apj) idels ks afilts
    zipWithM_ (\ref x -> writeSERef ref $ filt x) afiltRefs adels
    afilts      <- mapM readSERef afiltRefs
    return (mean $ odds afilts, mean $ evens afilts)    
    where
        idels = cycle $ fmap ( / getSampleRate) [2473, 2767, 3217, 3557, 3907, 4127, 2143, 1933]
        ks    = cycle $ zipWith3 (\a b c -> randi a b `withSeed` c) 
            [0.001, 0.0011, 0.0017, 0.0006, 0.001, 0.0011, 0.0017, 0.0006]
            [3.1,   3.5,    1.11,   3.973,  2.341, 1.897,  0.891,  3.221]
            [0.06,  0.9,    0.7,    0.3,    0.63,  0.7,    0.9,    0.44]

        del apj idel k afilt = do
            _ <- delayr 1
            adel1 <- deltapi $ sig idel + k * sig ipitchmod
            delayw $ asig  + apj - afilt
            return adel1

        filt adel = tone (adel * sig igain) (sig itone)

bassEnhancment :: D -> D -> Sig -> Sig
bassEnhancment cfq k asig = sig k * butlp asig (sig cfq) + asig

