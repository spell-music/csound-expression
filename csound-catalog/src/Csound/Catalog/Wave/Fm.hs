module Csound.Catalog.Wave.Fm(
    fmBass1, fmBass2
) where

import Csound.Base

fmBass1 :: MonoAdsr -> (Sig, Sig) -> Sig
fmBass1 env (amp, cps) = bhp 35 $ env 0.01 3 0.01 0.05 * (port amp 0.01) * (\x -> fosc 2 1 (1.5 * env 0.01 0.5 0.5 0.05) x + 0.4 * osc (x * 0.501)) (cps * (let env1 = env 1.2 0.1 0.85 5 * env 1.2 0.75 0.25 0.05 in 1 + (0.02 * env1 * uosc (3 * env1))))

fmBass2 :: MonoAdsr -> (Sig, Sig) -> Sig
fmBass2 adsrEnv (amp, cps) = env1 * (\freq -> fosc 1 1 (1 + 3.4 * env2) freq) ((cps * (1 + 0.001 * osc (2 * env4) * env4)))
    where
        env1 = adsrEnv 0.015 (5.2 + rel) 0.001 0.5
        env2 = adsrEnv 0.015 1.4 0.5 1.2        
        env4 = adsrEnv 3.2 0.1 0.85 5

        rel = 3 * (1 - (cps - 50) / 150)