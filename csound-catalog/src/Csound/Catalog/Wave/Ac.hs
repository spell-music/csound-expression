module Csound.Catalog.Wave.Ac(
    pulseWidth,
    xanadu1, xanadu2, stringPad, toneWheel,
    guitar, harpsichord, xing,
    fmMod, filteredChorus, plainString, fmTubularBell, 
    delayedString, melody, rhodes, 
) where

import Data.List

import Csound.Base
            
-- | 
--
-- > aout = pulseWidth amplitude cps
pulseWidth :: Sig -> Sig -> Sig
pulseWidth amp cps = asignal
    where
        ilforate  = 		2.3					-- LFO SPEED IN Hz
        isawlvl   = 		0.5	 				-- LEVEL OF SAWTOOTH WAVEFORM
        ipwmlvl   = 		0.5	 				-- LEVEL OF PULSE WAVEFORM
        ipwm	  = 		0.2	 				-- DC OFFSET OF PULSE width
        ipwmlfo   = 		0.1	 				-- DEPTH OF PULSE WIDTH MODULATION
        ivcffrq   = 		800	 				-- CUTOFF OF GLOBAL LOW PASS FILTER
        ienvflt   = 		200	 				-- MAX CHANGE IN LPF CUTOFF BY ENVELOPE
        ikbdflt   = 		0.1	 				-- RELATIVE CHANGE IN LPF CUTOFF TO PITCH
        -- the oscillators
        klfo        = kr $ osc ilforate
        asaw        = oscBy (elins [-1, 1]) cps 
        apwm        = table (0.5 + asaw / 2 + (klfo * ipwmlfo + ipwm)) (lins [-1, 50, -1, 0, 1, 50, 1]) `withD` 1
        awaves      = isawlvl * asaw + ipwmlvl * apwm
        -- the envelope
        -- the filters
        asignal     = amp * butlp awaves (ivcffrq + cps * ikbdflt + ienvflt * amp)

giwave :: Tab
giwave = sines [1, 0.5, 0.33, 0.25, 0.0, 0.1, 0.1, 0.1]

xanaduPlucks :: D -> D -> D -> (Sig, Sig, Sig)
xanaduPlucks cps vibrAmp vibrCps = (phi vib, phi shift, phi (-shift))
    where phi asig = pluck 1 (cpsoct $ oct + asig) cps giwave 1
          shift = 8/1200  
          vib = kr $ poscil (sig vibrAmp) (sig vibrCps) cosine
          oct = sig $ octcps cps

-- | 
--
-- > aout <- xanadu1 cps
xanadu1 :: D -> SE Sig
xanadu1 cps = do
    _ <- delayr 2    
    ~ [tap1, tap2, d1, d2] <- mapM deltap3 [f1, f2, 2, 1.1]
    delayw $ g * damping
    return $ damping * mean [gleft, tap1, d1, gright, tap2, d2]
    where (g, gleft, gright) = xanaduPlucks cps (1/120) (cps/50)
          f1 = expseg [0.01, 10, 1]
          f2 = expseg [0.015, 15, 1.055]
          damping = 1

-- | 
--
-- > aout <- xanadu2 cps
xanadu2 :: D -> SE Sig
xanadu2 cps = do
    _ <- delayr 0.4
    ~ [d1, d2] <- mapM deltap3 [0.07, 0.105]
    delayw $ g * damping
    return $ damping * mean [d1, gleft, d2, gright]
    where (g, gleft, gright) = xanaduPlucks cps (1/80) 6.1
          damping = 1

-- | 
--
-- > stringPad amplitude cps
stringPad :: Sig -> Sig -> Sig
stringPad amp cps = blp (900 + amp * 300) $ chorusPitch 3 0.1 f cps
    where f x = poscil 1 x giwave

-- | Tone wheel organ by Mikelson
--
-- > toneWheel cps
toneWheel :: D -> Sig
toneWheel cps = asignal
    where
        ikey = 12 * int' (cps - 6) + 100 * (cps - 6)
        wheels = 
            [ ifB (ikey - 12 >* 12) gitonewheel1 gitonewheel2
            , ifB (ikey +  7 >* 12) gitonewheel1 gitonewheel2
            , ifB (ikey      >* 12) gitonewheel1 gitonewheel2
            , sine ]
        iphase = 0.5
        harm w fqc tabId phs = poscil (sig w) (sig $ fqc * cps) (wheels !! tabId) `withD` (iphase / (ikey - phs))
        asignal = ( / 9) $ mean $ zipWith4 harm
            [8,   8,      8,   8,   3,      2,     1,      0, 4]
            [0.5, 1.4983, 1,   2,   2.9966, 4, 5.0397, 5.9932, 8]
            ([0, 1, 2, 3] ++ repeat 3)
            [-12, 7, 0, 12, 19, 24, 28, 31, 36]

        gitonewheel1 = sines [1, 0.02, 0.01]
        gitonewheel2 = sines [1, 0, 0.2, 0, 0.1, 0, 0.05, 0, 0.02]

-- | Guitar, Michael Gogins
--
-- > guitar cps
guitar :: D -> Sig
guitar cps = asignal
    where
        asigcomp    = pluck 1 440 440 def 1
        asig        = pluck 1 (sig cps) cps def 1
        af x cf wid = x * reson asig cf wid
        asignal     = balance (0.4 * asig + sum [af 0.6 110 80, af 1 220 100, af 0.6 440 80]) asigcomp

-- Harpsichord, James Kelley
--
-- > harpsicord cps
harpsichord :: D -> Sig
harpsichord cps = 0.5 * asignal
    where
        aenvelope   = ar $ transeg [1, 10, -5.0, 0]
        apluck      = pluck 1 (sig cps) cps def 1
        aharp       = poscil aenvelope (sig cps) (lins [-1, 1024, 1, 1024, -1])
        asignal     = apluck + balance apluck aharp

-- | Xing by Andrew Horner
--
-- > xing cycleDuration cps
xing :: D -> Sig -> Sig
xing xdur cps = asignal
    where
        amps xs dt vib freq phs = ar (loopseg xs (sig $ 1/xdur)) * (1 + poscil vibEnv freq sine `withD` phs)
            where vibEnv = ar $ loopseg [0, dt, vib, sig xdur - dt, 0] (sig $ 1/xdur)

        f vol freq = poscil vol (sig freq * cps) sine

        norm = 32310
        asignal = (sig $ 1 / norm) * sum 
            [ f (amps env1 0.05 0.3 6.7  0.8) 1
            , f (amps env2 0.12 0.5 10.5 0  ) 2.7
            , f (amps env3 0.02 0.8 70   0  ) 4.95
            ]
        
        env1 = [ 0,0.001,5200,0.001,800,0.001,3000,0.0025,1100,0.002
            , 2800,0.0015,1500,0.001,2100,0.011,1600,0.03,1400,0.95
            , 700,1,320,1,180,1,90,1,40,1,20,1,12,1,6,1,3,1,0,1,0]

        env2 = [ 0,0.0009,22000,0.0005,7300,0.0009,11000,0.0004,5500
            , 0.0006,15000,0.0004,5500,0.0008,2200,0.055,7300,0.02
            , 8500,0.38,5000,0.5,300,0.5,73,0.5,5,5,0,1,1]

        env3 = [ 0,0.001,3000,0.001,1000,0.0017,12000,0.0013
            , 3700,0.001,12500,0.0018,3000,0.0012,1200,0.001
            , 1400,0.0017,6000,0.0023,200,0.001,3000,0.001,1200
            , 0.0015,8000,0.001,1800,0.0015,6000,0.08,1200,0.2
            , 200,0.2,40,0.2,10,0.4,0,1,0]

-- | FM modulated left and right detuned chorusing, Thomas Kung
--
-- > fmMod cycleDuration cps
fmMod :: D -> Sig -> Sig
fmMod xdur cps = asignal
    where
        iattack     = 0.25
        irelease    = 0.3333
        ip6         = 0.3
        ip7         = 2.2
        ishift      = 4 / 12000
        ipch        = cps
        ioct        = octcps cps
        amodi       = ar $ loopseg [0, iattack, 5, sig xdur, 2, irelease, 0] (sig $ 1 / xdur) 
        amodr       = ar $ loopseg [ip6, 1, ip7, 1, ip6] (sig $ 0.5 / xdur) 
        a1          = amodi * (amodr - 1 / amodr) / 2
        a2          = amodi * (amodr + 1 / amodr) / 2
        a1ndx       = abs $ a1 / 10
        a3          = tablei a1ndx (skipNorm $ bessels 20) `withD` 1 
        ao1         = poscil a1 ipch cosine
        a4          = exp $ -0.5 * a3 + ao1
        ao2         = poscil (a2 * ipch) cps cosine
        aleft       = poscil a4 (ao2 + cpsoct (ioct + ishift)) sine
        aright      = poscil a4 (ao2 + cpsoct (ioct - ishift)) sine
        asignal     = 0.5 * (aleft + aright)         

-- | Filtered chorus, Michael Bergeman
--
-- > filteredChorus cycleDuration cps
filteredChorus :: D -> Sig -> Sig
filteredChorus xdur cps = asignal
    where 
        a ~~ b = loopseg [sig a, 1, sig b, 1, sig a] (sig $ 1 / (xdur * 2))
        filt cf1 bw1 cf2 bw2 x = balance (bp cf2 bw2 $ bp cf1 bw1 x) x
        harm fqc = poscil ((sig $ idb)) fqc $ sines 
                            [ 0.28, 1, 0.74, 0.66, 0.78, 0.48, 0.05, 0.33, 0.12
                            , 0.08, 0.01, 0.54, 0.19, 0.08, 0.05, 0.16, 0.01, 0.11, 0.3, 0.02, 0.2]
        a1s x = mean $ fmap (harm . (* cpsoct (octcps cps + x))) [1, 0.999, 1.001]
        rvb dt dh x = 0.5 * (x + reverb2 x dt dh) 

        idb = 1.5

        asignal = mean 
            [ rvb 5 0.3 $ filt (40 ~~ 800) 40 (220 ~~ 440) ((440 ~~ 220) * 0.8) $ a1s (-0.01)
            , rvb 4 0.2 $ filt (800 ~~ 40) 40 (440 ~~ 220) ((220 ~~ 440) * 0.8) $ a1s 0.01
            ]

-- | Plain plucked string, Michael Gogins
--
-- > plainString cps
plainString :: D -> Sig
plainString cps = wgpluck2 0.1 1.0 cps 0.25 0.05
   
-- | Rhodes electric piano model, Perry Cook
--
-- > rhodes cps
rhodes :: Sig -> Sig
rhodes cps = asignal
    where
        iindex      = 4.1
        icrossfade  = 3.1
        ivibedepth  = 0.2
        iviberate   = 6
        ifn1        = sine
        ifn2        = cosine
        ifn3        = sine
        ifn4        = sines [0]
        ivibefn     = sine
        asignal     = fmrhode 1 cps iindex icrossfade ivibedepth iviberate ifn1 ifn2 ifn3 ifn4 ivibefn

-- | Tubular bell model, Perry Cook
--
-- > fmTubularBell cps
fmTubularBell :: Sig -> Sig
fmTubularBell cps = asignal
    where
        iindex      = 1.5
        icrossfade  = 2.03
        ivibedepth  = 0.2
        iviberate   = 6
        ifn1        = sine
        ifn2        = sines [1, 0.4, 0.2, 0.1, 0.1, 0.05]
        ifn3        = sine
        ifn4        = sine
        ivibefn     = cosine
        asignal     = fmbell 1 cps iindex icrossfade ivibedepth iviberate `withTabs` [ifn1, ifn2, ifn3, ifn4, ivibefn]

-- | Delayed plucked string, Michael Gogins
--
-- > delayedString cps
delayedString :: D -> Sig
delayedString cps = asignal
    where
        ioctave     = octcps cps
        -- Detuning of strings by 4 cents each way
        idetune     = 4 / 1200
        kvibrato    = poscil (1 / 120) 7 sine
        awave det fn = pluck 1 (cpsoct $ sig ioctave + det) cps fn 1
        ag          = awave kvibrato  sine
        agleft      = awave idetune    sine
        agright     = awave (- idetune) cosine
        imsleft     = 0.2 * 1000
        imsright    = 0.21 * 1000
        noclick x   = linseg [0, 0.1, x, 1, x]
        adelayleft  = vdelay ag (noclick imsleft) (imsleft + 100)
        adelayright = vdelay ag (noclick imsright) (imsright + 100)
        asignal     = mean [agleft, adelayleft, agright, adelayright]


-- | Melody (Chebyshev / FM / additive), Jon Nelson
--
-- > melody cycleDuration cps
melody :: D -> Sig -> SE Sig
melody xdur cps = do
    k1000 <- randi 1 10
    let k100 = cps + loopseg [0, 0.5, 1, sig xdur, 1] (sig $ 1/xdur) * poscil 1 (5 + k1000) sine
        -- a1-3 are for cheby with p6=1-4
        a1   = poscil k1 k100 (sines [1, 0.4, 0.2, 0.1, 0.1, 0.05])
        a2   = tablei a1 ip6 `withDs` [1, 0.5]
        a3   = balance a2 a1
        -- try other waveforms as well
        a4          = foscil 1 (k100 + 0.04) 1 2.005 k20 sine
        a5          = poscil 1 k100 sine
        a6          = a3 * 0.1 + a4 * 0.1 + a5 * 0.8
        a7          = comb a6 0.5 (1 / ir cps)
        a8          = a6 * 0.9 + a7 * 0.1
        asignal     = balance a8 a1
    return asignal
    where
        iattack     = 0.05
        isustain    = xdur
        irelease    = 0.1
        ip6         = skipNorm $ lins [-1, 150, 0.1, 110, 0, 252, 0]
        -- Envelope for driving oscillator
        k1          = linseg [1, xdur, 0.5] * linenr 0.5 (xdur * 0.3) (xdur * 0.2) 0.01
        -- Power to partials
        k20         = linseg [1.485, iattack, 1.5, isustain + irelease, 1.485]

