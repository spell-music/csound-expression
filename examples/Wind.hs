-- | A whisper of the wind.
module Main where

import Csound

-------------------------------------------------------------
-- orchestra

-- | An instrument that implements a wind sound.
wind (amp, bandRise, bandDec, freqRise, freqDec, pan, winds) = 
    fmap fromRnd $ rand (sig $ amp / 400)
    where
        valu1 = 100
        valu2 = 50
        winde = 1 - winds
        ramp a b = linseg [a, idur, b]
        fromRnd a = (sig pan * aout, (1 - sig pan) * aout )
            where
                a2 = butbp a  (ramp freqRise freqDec) (ramp bandRise bandDec) 
                a3 = butbp a2 (ramp (freqRise - valu1) (freqDec + valu2))
                              (ramp (bandRise + valu1) (bandDec - valu2))
                
                aout = (a2 + a3) * linseg [0, idur * winds, 1, idur * winde, 0]

-------------------------------------------------------------
-- scores

-- | This example shows the flexibility of the Haskell. Here we can
-- write the scores in the manner of the native Csound style.
i21 t0 dt amp bandRise bandDec freqRise freqDec pan winds = 
    del t0 $ dt *| temp (amp, bandRise, bandDec, freqRise, freqDec, pan, winds)

windRes = sco (onArg wind) $ har
    [ i21  0    12   60   500  0    555  111  0     0.2
    , i21  4    8    70   400  0    444  111  0.3   0.6
    , i21  6    8    75   300  0    333  111  0.7   0.7
    , i21  8.5  7    77   300  0    222  111  0.9   0.9

    , i21  12   12   70   500  0    666  444  0.1   0.4
    , i21  15   15   76   600  0    333  333  0.5   0.7
    , i21  17   7    73   500  0    444  333  0.7   0.7
    , i21  18   7    70   400  0    333  333  0.9   0.8

    , i21  20   12   72   500  0    555  333  0.0   0.4
    , i21  24   8    74   600  0    444  333  0.1   0.6
    , i21  28   6    71   500  0    333  333  0.5   0.7
    , i21  28.5 7    70   71   500  666  333  0.9   0.7

    , i21  32   12   76   500  0    666  222  0.1   0.6
    , i21  35   15   72   500  0    333  333  0.5   0.7
    , i21  37   7    74   600  0    444  333  0.7   0.7
    , i21  38   7    72   300  0    333  333  0.9   0.8

    , i21  40   15   70   500  0    555  111  0     0.4
    , i21  44   10   74   600  0    444  111  0.5   0.6
    , i21  48   8    77   500  0    333  111  0.7   0.7
    , i21  48.5 9    76   500  0    222  111  0.9   0.9 

    , i21  50   12   72   500  0    666  222  0.1   0.6
    , i21  55   15   74   500  0    333  333  0.5   0.7
    , i21  58   8    73   600  0    444  333  0.7   0.7
    , i21  65   9    72   300  0    333  333  0.9   0.8

    , i21  58   12   74   500  0    666  222  0.1   0.6
    , i21  61   15   76   500  0    333  333  0.5   0.7
    , i21  63   12   73   600  0    444  333  0.7   0.7
    , i21  64   12   72   300  0    333  333  0.9   0.8
    ]

-- | Let's repeat everything 10 times and add the sustain for 2 seconds per note.
main = dac $ mix $ sustain 2 $ loop 3 windRes

