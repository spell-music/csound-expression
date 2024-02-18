module WindTest where

import Csound.Base

import Wind

main' = dac $ do
    asig <- midi 1 instrM
    return $ 0.5 * rvbInstr 1.5 0.7 asig

main = dac $ do
    asig <- runMix $ sco instr $ CsdEventList 7 
        [ (0, 5, (0.5, 220))
        , (0, 5, (0.2, 220*5/4)) 
        , (0, 5, (0.2, 330)) 
        , (0, 5, (0.9, 440)) 
        ]
    return $ 0.2 * rvbInstr 1.5 0.7 asig


instrM :: Msg -> Sig
instrM msg = sig amp * env * wave 
    where 
        (amp, cps) = (ampmidi msg, cpsmidi msg)
        env = linsegr [1, 1, 1] 0.5 0
        wave = flute 0.5 cps 0.01 0.06 5 0.15 7

instr :: (D, D) -> Sig
instr (seed, cps) = frenchHorn seed cps 0.01 0.06 (idur - 0.31) 0.25 5

rvbInstr :: D -> D -> Sig -> Sig
rvbInstr irevtime ireverb asig = sig ireverb * aout
    where aout = reverb asig (sig irevtime)

