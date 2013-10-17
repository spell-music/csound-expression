-- | Test for different types of events.
module Main where

import Csound.Base
import Data.Monoid

-- | Outputs the drummy sound.
pureTone :: (D, D) -> SE Sig
pureTone (amp, cps) = do
    print' [amp, cps]
    return $ env * sig amp * osc (sig cps)
    where env = expseg [1, idur, 1e-6]

-- | Just prints the argument on the screen.
echo :: D -> SE ()
echo a = print' [a]


-- | A single metronome (four beats per second).
src = metroE 4

e1, e2, e3, e4, e5, e6, e7, e8 :: Evt D

e1 = cycleE (fmap int [1 .. 5]) src

e2 = iterateE 0 (+1) src

e3 = fmap (flip mod' 4) e2

e4 = appendE 0 (+) $ repeatE 1 src

e5 = mappendE e2

e51 :: Evt (D, D)
e51 = mappend 
        (repeatE (0.7, 220) (metroE (2/7)))
        (cycleE [(0.5, 440), (0.5, 330)] (metroE 2))

-- rands

e6 = randDs src

e7 = randInts (0, 10) src

e8 = oneOf [100, 200, 300] src

e9 = avgSum $ freqOf [(0.1, 0), (0.9, 1)] src

avgSum :: Evt D -> Evt D
avgSum = accumE (0, 0) $ \a (s, n) ->     
    let s1 = s + a
        n1 = n + 1
    in  (s1 / n1, (s1, n1))

-- mask

e10 :: Evt (D, D)
e10 = fmap (\x -> (0.5, x)) $ filterE (>* 110) $ mconcat 
    [ every 0 [5,7] $ repeatE 330 src
    , every 3 [11] $ repeatE 550 src
    , every 2 [2] $ repeatE 440 src
    , every 0 [4, 1, 3] $ repeatE 220 src]

-----------------------

res e = sched_ echo $ fmap (\a -> (0.1, a)) $ e

-- output with sound
resSnd e = sched pureTone $ fmap (\a -> (0.1, a)) e

main = dac_ $ res e1

