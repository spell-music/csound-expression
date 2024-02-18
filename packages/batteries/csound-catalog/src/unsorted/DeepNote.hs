module Main where

import Csound.Base

deepNote :: D -> D -> D -> D -> D -> SE Sig
deepNote dt1 dt2 dt3 cps1 cps2 = fmap asig kjit
    where
        dt      = dt1 + dt2 + dt3
        kpch    = linseg [cps1, dt1, cps1, dt2, cps2, dt3, cps2]
        kjit    = fmap (linseg [1, dt / 2, 0, 1, 0] * ) $ jitter 30 1 3
        asig jit = a1 -- moogvcf a1 10000 0.1
            where a1 = vco2 1 (kpch + jit) `withDs` [4, 0.5]

instr msg = fmap ((amp * env) * ) $ deepNote 13 3 8 cps (cps * 3/2)
    where     
        (amp, cps) = (sig $ ampmidi msg, cpsmidi msg)
        env = linseg [0, 13, 0.5, 3, 1, 4, 1, 4, 0]

main = dac $ midi 1 instr    

