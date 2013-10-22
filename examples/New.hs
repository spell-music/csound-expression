module New where

import Csound.Base

ping (amp, cps) = sig amp * linseg [0, 0.1, 1, 1, 0] * osc (sig cps)

user :: Msg -> Sig
user msg = ($ (amp, cps)) $ schedHarpBy ping $ \x -> repeatE x $ metroE 0.00001
    where (amp, cps) = ampCps msg

instr a = do
    ref <- newSERef (0 :: D)
    writeSERef ref 2
    b <- readSERef ref
    print' [a + b]

scos = CsdEventList 1 [(0, 1, 1)]

e1 :: Evt D
e1 = mappendE $ repeatE 1 $ metroE 2 
    where 
        e = iterateE (0::D) (+1) $ metroE 1
        

echo a = print' [a]

res e = sched_ echo $ fmap (\a -> (0.1, a)) e

main = dac $ schedHarp ping $ fmap (\x -> (0.5, x)) $ cycleE [440, 330, 220] $ metroE 4

