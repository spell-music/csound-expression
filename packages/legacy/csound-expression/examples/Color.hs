-- | A gallery of instruments (found in Csound catalog).
module Color where

import Csound.Base

bass (amp, cps) = sig amp * once env * osc (sig cps)
  where
    env = eexps [1, 0.00001]

pluckSynth (amp, cps1, cps2) = 0.5 * sig amp * once env * pluck 1 (sig cps1) cps2 def 3
  where
    env = eexps [1, 0.004]

marimbaSynth :: (D, D) -> Sig
marimbaSynth (amp, cps) = a6
  where
    bias = 0.11
    i2 = log cps / 10 - bias
    k1 = (0.1 *) $ once $ lins [0.00001, 30, 1, 50, 0.5, 100, 0.00001]
    --        k1  = expseg [0.0001, 0.03, amp * 0.7, idur - 0.03, 0.001]
    --            * linseg [1, 0.03, 1, idur - 0.03, 3]
    k10 = linseg [2.25, 0.03, 3, idur - 0.03, 2]
    a1 = gbuzz k1 (sig cps) k10 0 35 (sines3 [(1, 1, 90)])
    a2 = reson' a1 500 50
    a3 = reson' a2 150 100
    a4 = reson' a3 3500 150
    a5 = reson' a4 3500 150
    a6 = balance a5 a1
    reson' a b c = reson a b c `withD` 1

phasing (amp, cps) = aout
  where
    rise = 1
    dec = 0.5

    env = linen (sig amp) rise idur dec
    osc' tab k ph = oscBy tab (sig $ cps * k) `withD` ph
    osc1 = osc' sine
    osc2 = osc' $ sines [1, 0, 0.9, 0, 0.8, 0, 0.7, 0, 0.6, 0, 0.5, 0, 0.4, 0, 0.3, 0, 0.2, 0, 0.1]
    asum =
      env
        * mean
          [ osc1 1 0
          , osc2 1.008 0.02
          , osc1 0.992 0.04
          , osc2 2 0.06
          , osc2 1 0.08
          , osc1 1 0.01
          ]
    kosc1 = 0.5 * once sine
    kosc2 = 0.5 * once sine `withD` 0.4
    kosc3 = 1.0 * once sine `withD` 0.8

    afilt =
      sum
        [ butbp asum kosc1 1000
        , butbp asum kosc2 300
        , butbp asum kosc3 20
        ]

    aout = mean [afilt, asum]

blurp amp = do
  cps <- acps
  return $ 0.1 * sig amp * osc cps
  where
    dec = linseg [11, idur * 0.75, 11, idur * 0.25, 0]
    kgate = kr $ oscil 1 dec (elins [1, 0, 0])
    anoise = noise 11000 0.99
    acps = fmap (flip samphold kgate) anoise

wind (amp, bandRise, bandDec, freqRise, freqDec, pan, winds) =
  fmap fromRnd $ rand (sig $ amp / 400)
  where
    valu1 = 100
    valu2 = 50
    winde = 1 - winds
    ramp a b = linseg [a, idur, b]
    fromRnd a = (sig pan * aout, (1 - sig pan) * aout)
      where
        a2 = butbp a (ramp freqRise freqDec) (ramp bandRise bandDec)
        a3 =
          butbp
            a2
            (ramp (freqRise - valu1) (freqDec + valu2))
            (ramp (bandRise + valu1) (bandDec - valu2))

        aout = (a2 + a3) * linseg [0, idur * winds, 1, idur * winde, 0]

noiz (amp, cps) = fmap a2 k2
  where
    k1 = linseg [1, 0.05, 100, 0.2, 100, 2, 1, idur, 1]
    k2 = fmap (`withD` 1) $ rand 500

    buzz' kamp kcps = buzz kamp (sig $ kcps * cps) k1 sine

    a1 = mean $ zipWith buzz' [0.3, 1, 1] [1, 0.5, 0.501]
    a2 k = 0.5 * a1 * sig amp * osc k

noiseGliss (amp, cps) = fmap ares anoise
  where
    ramp = linseg [0, idur * 0.8, amp, idur * 0.2, 0]
    env1 = linen (sig amp) 0 idur 10
    anoise = randi ramp env1

    ares a = reson (a * osc (sig cps)) 100 100 `withD` 2

ivory (amp, cps, vibRate, glisDur, cpsCoeff) =
  sig amp
    * mean
      --    vibrato env                amplitude env               freq bias   phase   vibrato coeff   wave
      [ alg (linseg [0, idur, 5]) (lincone 0 0.7 1 0.3 0) 0 0 1 sine
      , alg (lincone 0 0.6 6 0.4 0) (lincone 0 0.9 1 0.1 0) 0.009 0.2 0.9 (sines [10, 9 .. 1])
      , alg (lincone 9 0.7 1 0.3 1) (linenIdur 0.5 0.333) 0.007 0.3 1.2 (sines [10, 0, 9, 0, 8, 0, 7, 0, 6, 0, 5])
      , alg
          (expcone 1 0.4 3 0.6 0.02)
          (expcone 0.0001 0.8 1 0.2 0.0001)
          0.005
          0.5
          0.97
          (sines [10, 10, 9, 0, 0, 0, 3, 2, 0, 0, 1])
      , alg
          (expcone 1 0.4 3 0.6 0.02)
          (expdur [0.001, 0.5, 1, 0.1, 0.6, 0.2, 0.97, 0.2, 0.001])
          0.003
          0.8
          0.99
          (sines [10, 0, 0, 0, 5, 0, 0, 0, 0, 0, 3])
      , alg
          (expcone 4 0.91 1 0.09 1)
          (expdur [0.001, 0.6, 1, 0.2, 0.8, 0.1, 0.98, 0.1, 0.001])
          0.001
          1.3
          1.4
          (sines [10, 0, 0, 0, 0, 3, 1])
      ]
  where
    alg :: Sig -> Sig -> D -> D -> D -> Tab -> Sig
    alg vibrEnv ampEnv cpsBias phsBias vibrCoeff wave =
      ampEnv * (oscBy wave ((sig (cps + cpsBias) + vibr) * glis) `withD` phsBias)
      where
        glis = expseg [1, glisDur, 1, idur - glisDur, cpsCoeff]
        vibr = vibrEnv * osc (sig $ vibRate * vibrCoeff)

    cone a x1 b x2 c = [a, x1 * idur, b, x2 * idur, c]
    lincone a x1 b x2 c = linseg $ cone a x1 b x2 c
    expcone a x1 b x2 c = expseg $ cone a x1 b x2 c
    linenIdur a b = linen 1 (a * idur) idur (b * idur)

-- snow flakes
blue (amp, cps, lfoCps, harmNum, sweepRate) = fmap aout k1
  where
    k1 = randi 1 50
    k2 = lindur [0, 0.5, 1, 0.5, 0]
    k3 = lindur [0.005, 0.71, 0.015, 0.29, 0.01]
    k4 = k2 * (kr $ osc (sig lfoCps) `withD` 0.2)
    k5 = k4 + 2

    ksweep = lindur [harmNum, sweepRate, 1, 1 - sweepRate, 1]
    kenv = expdur [0.001, 0.01, amp, 0.99, 0.001]
    aout k = gbuzz kenv (sig cps + k3) k5 ksweep k (sines3 [(1, 1, 90)])

i2 t0 dt amp cps lfoCps harmNum sweepRate =
  t0 +| (dt *| temp (amp, cps, lfoCps, harmNum, sweepRate))

blueSco =
  sco blue $
    har
      [ i2 0 4 0.5 440 23 10 0.72
      , i2 0 4 0.5 330 20 6 0.66
      ]

-- 4pok
black (amp, cps, filterSweepStart, filterSweepEnd, bandWidth) =
  fmap aout $ rand 1
  where
    k1 = expdur [filterSweepStart, 1, filterSweepEnd]
    a1 noise = reson noise k1 (k1 / sig bandWidth) `withD` 1
    k3 = expdur [0.001, 0.001, amp, 0.999, 0.001]
    a2 = k3 * osc (sig cps + 0.6 * (osc 11.3 `withD` 0.1))
    aout noise = 0.5 * (a1 noise + a2)

i4 t0 dt amp cps filt0 filt1 bw =
  t0 +| (dt *| temp (amp, cps, filt0, filt1, bw))

blackSco =
  str 1.5 $
    sco black $
      har
        [ i4 0 1 0.7 220 6000 30 10
        , i4 0 1 0.7 330 8000 20 6
        ]
