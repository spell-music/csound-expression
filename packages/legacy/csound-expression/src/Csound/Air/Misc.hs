{-# LANGUAGE FlexibleContexts #-}

-- | Patterns
module Csound.Air.Misc (
  mean,
  vibrate,
  randomPitch,
  chorusPitch,
  resons,
  resonsBy,
  modes,
  dryWet,
  once,
  onceBy,
  several,
  fromMono,

  -- * List functions
  odds,
  evens,

  -- * Random functions
  rndPan,
  rndPan2,
  rndVol,
  gaussVol,

  -- * Choose signals
  selector,

  -- * Saving to file
  writeHifi,

  -- * Arpeggios
  arpeggi,
  arpBy,

  -- * GUI
  lpJoy,

  -- * Effects
  delaySig,
  delay1k,

  -- * Wave shaper
  wshaper,
  genSaturator,
  saturator,
  mildSaturator,
  hardSaturator,
  hardSaturator2,

  -- * Function composition
  funSeq,
  funPar,

  -- * Metronome
  ticks,
  nticks,
  ticks2,
  nticks2,
  ticks3,
  nticks3,
  ticks4,
  nticks4,

  -- * Drone
  testDrone,
  testDrone2,
  testDrone3,
  testDrone4,

  -- * Attack detection
  attackTrig,
  attackTrigSig,

  -- * Ambient guitar FX
  ambiEnv,
) where

import Data.Boolean
import Data.Default

import Csound.Air.Envelope
import Csound.Air.Filter
import Csound.Air.Patch
import Csound.Air.Wave
import Csound.Control.Evt
import Csound.Control.Gui
import Csound.Control.Instr
import Csound.IO (writeSndBy)
import Csound.Options (setRates)
import Csound.Tab
import Csound.Typed
import Csound.Typed.Opcode hiding (metro)
import Csound.Typed.Plugins (delay1k)

--------------------------------------------------------------------------
-- patterns

-- | Selects odd elements from the list.
odds :: [a] -> [a]
odds as = fmap snd $ filter fst $ zip (cycle [True, False]) as

-- | Selects even elements from the list.
evens :: [a] -> [a]
evens as
  | null as = []
  | otherwise = odds $ tail as

-- | Reads table once during the note length.
once :: Tab -> Sig
once = onceBy idur

-- | Reads table once during a given period of time.
onceBy :: D -> Tab -> Sig
onceBy dt tb = kr $ oscBy tb (1 / sig dt)

-- | Reads table several times during the note length.
several :: Tab -> Sig -> Sig
several tb rate = kr $ oscil3 1 (rate / sig idur) tb

-- | Mean value.
mean :: (Fractional a) => [a] -> a
mean xs = sum xs / (fromIntegral $ length xs)

-- | Adds vibrato to the sound unit. Sound units is a function that takes in a frequency.
vibrate :: Sig -> Sig -> (Sig -> a) -> (Sig -> a)
vibrate vibDepth vibRate f cps = f (cps * (1 + kvib))
  where
    kvib = vibDepth * kr (osc vibRate)

-- | Adds a random vibrato to the sound unit. Sound units is a function that takes in a frequency.
randomPitch :: Sig -> Sig -> (Sig -> a) -> (Sig -> SE a)
randomPitch randAmp randCps f cps = fmap go $ randh (cps * randAmp) randCps
  where
    go krand = f (cps + krand)

-- | Chorus takes a number of copies, chorus width and wave shape.
chorusPitch :: Int -> Sig -> (Sig -> Sig) -> Sig -> Sig
chorusPitch n wid = phi dts
  where
    phi :: [Sig] -> (Sig -> Sig) -> Sig -> Sig
    phi ks f = \cps -> mean $ fmap (f . (+ cps)) ks

    dts = fmap (\x -> -wid + fromIntegral x * dt) [0 .. n - 1]

    dt = 2 * wid / fromIntegral n

{- | Applies a resonator to the signals. A resonator is
a list of band pass filters. A list contains the parameters for the filters:

> [(centerFrequency, bandWidth)]
-}
resons :: [(Sig, Sig)] -> Sig -> Sig
resons = resonsBy bp

{- | A resonator with user defined band pass filter.
Warning: a filter takes in a center frequency, band width and the signal.
The signal comes last (this order is not standard in the Csound but it's more
convinient to use with Haskell).
-}
resonsBy :: (cps -> bw -> Sig -> Sig) -> [(cps, bw)] -> Sig -> Sig
resonsBy flt ps asig = mean $ fmap (($ asig) . uncurry flt) ps

{- | Mixes dry and wet signals.

> dryWet ratio effect asig

* @ratio@ - of dry signal to wet

* @effect@ - means to wet the signal

* @asig@ -- processed signal
-}
dryWet :: Sig -> (Sig -> Sig) -> Sig -> Sig
dryWet k ef asig = k * asig + (1 - k) * ef asig

{- | Chain of mass-spring-damping filters.

> modes params baseCps exciter

* params - a list of pairs @(resonantFrequencyRatio, filterQuality)@

* @baseCps@ - base frequency of the resonator

* exciter - an impulse that starts a resonator.
-}
modes :: [(Sig, Sig)] -> Sig -> Sig -> Sig
modes = relResonsBy (\cf q asig -> mode asig cf q)

relResonsBy :: (Sig -> a -> Sig -> Sig) -> [(Sig, a)] -> Sig -> Sig -> Sig
relResonsBy resonator ms baseCps apulse = (recip normFactor *) $ sum $ fmap (\(cf, q) -> harm cf q apulse) ms
  where
    -- limit modal frequency to prevent explosions by
    -- skipping if the maximum value is exceeded (with a little headroom)
    gate :: Sig -> Sig
    gate cps = ifB (sig getSampleRate >* pi * cps) 1 0

    normFactor = sum $ fmap (gate . (* baseCps) . fst) ms

    -- an ugly hack to make filter stable for forbidden values)
    harm cf q x = g * resonator (1 - g + g * cps) q x
      where
        cps = cf * baseCps
        g = gate cps

-- | Doubles the mono signal to get the stereo signal.
fromMono :: Sig -> (Sig, Sig)
fromMono a = (a, a)

-- | Random panning
rndPan2 :: Sig2 -> SE Sig2
rndPan2 (a, b) = rndPan $ mean [a, b]

-- | Random panning
rndPan :: Sig -> SE Sig2
rndPan a = do
  fmap (pan2 a . sig) (rnd (1 :: D))

{- | Random volume (with gauss distribution)

> gaussVol radiusOfDistribution
-}
gaussVol :: (SigSpace a) => D -> a -> SE a
gaussVol k a = do
  level <- fmap ir $ gauss (sig k)
  return $ mul (sig $ level + 1) a

{- | Random volume

> gaussVol (minVolume, maxVolume)
-}
rndVol :: (SigSpace a) => (D, D) -> a -> SE a
rndVol (kMin, kMax) a = do
  level <- rnd (1 :: D)
  return $ mul (sig $ kMin + (kMax - kMin) * level) a

{- | Hi-fi output for stereo signals. Saves the stereo signal to file.
The length of the file is defined in seconds.

> writeHifi fileLength fileName asig
-}
writeHifi :: D -> String -> SE Sig2 -> IO ()
writeHifi n fileName a = writeSndBy (setRates 48000 10) fileName $ fmap (setDur $ n) a

{- | It picks a signal from the list by integer index.
The original value is taken from the head of the list (the first element).
-}
selector :: (Num a, SigSpace a) => [a] -> Sig -> a
selector as k = sum $ zipWith choice [0 ..] as
  where
    choice n a = mul (port (ifB (sig (int n) ==* k) 1 0) 0.02) a

{- | Creates running arpeggios.

> arpeggiBy ampWeights pitches instrument cps

It plays an instrument with fast sequence of notes. We can specify
the pitches and amplitude weights of the notes as well as frequency of repetition.
-}
arpeggi :: (SigSpace a) => [Sig] -> [Sig] -> (Sig -> a) -> Sig -> a
arpeggi = arpBy triSeq sqrSeq

{- | Creates running arpeggios.

> arpeggiBy ampWave pitchwave ampWeights pitches instrument cps

It plays an instrument with fast sequence of notes. We can specify amplitude envelope wave, pitch envelope wave,
the pitches and amplitude weights of the notes as well as frequency of repetition.
-}
arpBy :: (SigSpace a) => ([Sig] -> Sig -> Sig) -> ([Sig] -> Sig -> Sig) -> [Sig] -> [Sig] -> (Sig -> a) -> Sig -> a
arpBy ampWave cpsWave amps cpss wave dt = mul (ampWave amps dt) $ wave $ cpsWave cpss dt

{- | Low-pass filter pictured as joystick.
Ox is for center frequency and Oy is for resonance.
-}
lpJoy :: Source (Sig -> Sig)
lpJoy = lift1 (\(cps, res) -> mlp cps res) $ joy (expSpan 100 17000) (linSpan 0.05 0.95) (1400, 0.5)

-- | Chains all functions in the list.
funSeq :: [a -> a] -> a -> a
funSeq = foldl (.) id

{- | Applies all functions in the list to the given input
and summs them up.
-}
funPar :: (Num a) => [a -> a] -> a -> a
funPar fs a = sum $ fmap ($ a) fs

{- | Delay a signal by certain number of seconds
There is a subtle difference between the function and the function @delaySnd@.
The @delaySig@ is for delaying a signal on a micro level (the delay time have to be small)
It's implemented with delay buffer in the csound. But @delaySnd@ is for delaying
on macro level (the delay time can be big). It's implemented with scores and invocation
of hidden instruments.

> delaySig numOfSamples asig
-}
delaySig :: D -> Sig -> Sig
delaySig nsamples asig = delay asig nsamples

-----------------------------------------------------
-- metronome

-- It contains a small copy of Csouns.Catalog.Tr808. Just enough to implement a metronome.

data TrSpec = TrSpec
  { trDur :: D
  , trTune :: D
  , trCps :: D
  , trRnd :: Maybe D
  }

rndAmp :: Sig -> SE Sig
rndAmp a = do
  k <- birnd 0.09
  return $ a * (1 + sig k)

rndVal :: D -> D -> D -> SE D
rndVal total amount x = do
  k <- birnd amount
  return $ x + k * total

rndDur, rndCps, rndTune :: D -> D -> SE D
rndDur amt x = rndVal x amt x
rndCps amt x = rndVal x (amt / 10) x
rndTune amt x = rndVal 0.7 amt x

rndSpec :: TrSpec -> SE TrSpec
rndSpec spec = do
  dur' <- rndDur'
  tune <- rndTune'
  cps <- rndCps'
  return $
    spec
      { trDur = dur'
      , trTune = tune
      , trCps = cps
      }
  where
    rndDur' = (maybe return rndDur $ (trRnd spec)) $ trDur spec
    rndTune' = (maybe return rndTune $ (trRnd spec)) $ trTune spec
    rndCps' = (maybe return rndCps $ (trRnd spec)) $ trCps spec

addDur' :: D -> Sig -> SE Sig
addDur' dt x = xtratim dt >> return x

addDur :: Sig -> SE Sig
addDur = addDur' 0.1

getAccent :: Int -> [D]
getAccent n = 1 : replicate (n - 1) 0.5

{- | Metronome with a chain of accents.
A typical 7/8 for example:

> dac $ nticks [3, 2, 2] (135 * 2)
-}
nticks :: [Int] -> Sig -> Sig
nticks = nticks' rimShot'

nticks2 :: [Int] -> Sig -> Sig
nticks2 = nticks' claves'

nticks3 :: [Int] -> Sig -> Sig
nticks3 = nticks' maraca'

nticks4 :: [Int] -> Sig -> Sig
nticks4 = nticks' highConga'

nticks' :: (TrSpec -> SE Sig) -> [Int] -> Sig -> Sig
nticks' drum ns = genTicks drum (cycleE $ ns >>= getAccent)

{- | Metronome.

> ticks n bpm
-}
ticks :: Int -> Sig -> Sig
ticks = ticks' rimShot'

ticks2 :: Int -> Sig -> Sig
ticks2 = ticks' claves'

ticks3 :: Int -> Sig -> Sig
ticks3 = ticks' maraca'

ticks4 :: Int -> Sig -> Sig
ticks4 = ticks' highConga'

ticks' :: (TrSpec -> SE Sig) -> Int -> Sig -> Sig
ticks' drum n
  | n <= 1 = genTicks drum (devt 0.5)
  | otherwise = genTicks drum (cycleE $ getAccent n)

genTicks :: (TrSpec -> SE Sig) -> (Tick -> Evt D) -> Sig -> Sig
genTicks drum f x =
  mul 3 $
    mlp 4000 0.1 $
      sched (\amp -> mul (sig amp) $ drum (TrSpec (amp + 1) 0 (1200 * (amp + 0.5)) (Just 0.05))) $
        withDur 0.5 $
          f $
            metro (x / 60)

rimShot' :: TrSpec -> SE Sig
rimShot' spec = pureRimShot' =<< rndSpec spec

-- cps = 1700
pureRimShot' :: TrSpec -> SE Sig
pureRimShot' spec = rndAmp =<< addDur =<< (mul 0.8 $ aring + anoise)
  where
    dur' = trDur spec
    tune = trTune spec
    cps = trCps spec

    fullDur = 0.027 * dur'

    -- ring
    aenv1 = expsega [1, fullDur, 0.001]
    ifrq1 = sig $ cps * octave tune
    aring = mul (0.5 * (aenv1 - 0.001)) $ at (bbp ifrq1 (ifrq1 * 8)) $ rndOscBy tabTR808RimShot ifrq1

    -- noise
    aenv2 = expsega [1, 0.002, 0.8, 0.005, 0.5, fullDur - 0.002 - 0.005, 0.0001]
    kcf = expsegr [4000, fullDur, 20] fullDur 20
    anoise = mul (aenv2 - 0.001) $ fmap (blp kcf) $ noise 1 0

    tabTR808RimShot = setSize 1024 $ sines [0.971, 0.269, 0.041, 0.054, 0.011, 0.013, 0.08, 0.0065, 0.005, 0.004, 0.003, 0.003, 0.002, 0.002, 0.002, 0.002, 0.002, 0.001, 0.001, 0.001, 0.001, 0.001, 0.002, 0.001, 0.001]

claves' :: TrSpec -> SE Sig
claves' spec = rndAmp =<< addDur =<< asig
  where
    dur' = trDur spec
    tune = trTune spec
    cps = trCps spec

    ifrq = cps * octave tune
    dt = 0.045 * dur'
    aenv = expsega [1, dt, 0.001]
    afmod = expsega [3, 0.00005, 1]
    asig = mul (-0.4 * (aenv - 0.001)) $ rndOsc (sig ifrq * afmod)

highConga' :: TrSpec -> SE Sig
highConga' = genConga 0.22

genConga :: D -> TrSpec -> SE Sig
genConga dt spec = rndAmp =<< addDur =<< asig
  where
    dur' = trDur spec
    tune = trTune spec
    cps = trCps spec

    ifrq = cps * octave tune
    fullDur = dt * dur'
    aenv = transeg [0.7, 1 / ifrq, 1, 1, fullDur, -6, 0.001]
    afmod = expsega [3, 0.25 / ifrq, 1]
    asig = mul (-0.25 * aenv) $ rndOsc (sig ifrq * afmod)

maraca' :: TrSpec -> SE Sig
maraca' spec = rndAmp =<< addDur =<< anoise
  where
    dur' = trDur spec
    tune = trTune spec

    otune = sig $ octave tune
    iHPF = limit (6000 * otune) 20 (sig getSampleRate / 2)
    iLPF = limit (12000 * otune) 20 (sig getSampleRate / 3)
    aenv = expsega [0.4, 0.014 * dur', 1, 0.01 * dur', 0.05, 0.05 * dur', 0.001]
    anoise = mul aenv $ fmap (blp iLPF . bhp iHPF) $ noise 0.75 0

-------------------------------------------
-- drones (copied from csound-catalog)

testDrone, testDrone2, testDrone3, testDrone4 :: D -> SE Sig2
testDrone cps = atNote (deepPad razorPad) (0.8, cps)
  where
    razorPad = razorPad' def
    razorPad' (RazorPad speed) = withLargeHall' 0.35 $ polySynt $ at fromMono . mul 0.6 . onCps (uncurry $ impRazorPad speed)
testDrone2 cps = atNote (deepPad nightPad) (0.8, cps)
  where
    nightPad = withLargeHall $ polySynt $ mul 0.48 . at fromMono . onCps (mul (fadeOut 1) . impNightPad 0.5)
testDrone3 cps = atNote (deepPad caveOvertonePad) (0.8, cps)
  where
    caveOvertonePad = FxChain (fx1 0.2 (magicCave2 . mul 0.8)) $ polySynt overtoneInstr
testDrone4 cps = atNote (deepPad pwEnsemble) (0.8, cps)
  where
    pwEnsemble = withSmallHall $ polySynt $ at fromMono . mul 0.55 . onCps impPwEnsemble

data RazorPad = RazorPad Sig

instance Default RazorPad where
  def = RazorPad 0.5

overtoneInstr :: CsdNote D -> SE Sig2
overtoneInstr = mul 0.65 . at fromMono . mixAt 0.25 (mlp 1500 0.1) . onCps (\cps -> mul (fades 0.25 1.2) (tibetan 11 0.012 cps) + mul (fades 0.25 1) (tibetan 13 0.015 (cps * 0.5)))

-- implem

impPwEnsemble :: Sig -> SE Sig
impPwEnsemble x =
  mul 0.3 $
    at (mlp (3500 + x * 2) 0.1) $
      mul (leg 0.5 0 1 1) $
        sum
          [ f 0.2 0.11 2 (x * cent (-6))
          , f 0.8 (-0.1) 1.8 (x * cent 6)
          , f 0.2 0.11 2 (x * 0.5)
          ]
  where
    f a b c = rndPw (a + b * tri c)

{- | Tibetan chant. It's a chorus of many sinusoids.

> tibetan n off cps

* n - the number of sinusoids (the best is 9)

* off - frequency step of the harmonics ~ (0.01, 0.03)

* cps - the frequency of the note
-}
tibetan :: Int -> Sig -> D -> Sig
tibetan n off cps = chorusPitch n (2 * off * fromIntegral n) (oscBy wave) (sig cps)
  where
    wave = ifB (cps `lessThan` 230) (waveBy 5) (ifB (cps `lessThan` 350) (waveBy 3) (waveBy 1))
    waveBy x = sines $ [0.3, 0, 0, 0] ++ replicate x 0.1

impRazorPad :: Sig -> Sig -> Sig -> SE Sig
impRazorPad speed' amp' cps' = g cps' + 0.75 * g (cps' * 0.5)
  where
    g cps = mul (leg 0.5 0 1 1) $ genRazor (filt 1 mlp) speed' amp' cps

    genRazor f speed amp cps = mul amp $ do
      a1 <- ampSpline 0.01
      a2 <- ampSpline 0.02

      return $
        f (1000 + 2 * cps + 500 * amp) 0.1 $
          mean
            [ fosc 1 3 (a1 * uosc (speed)) cps
            , fosc 3 1 (a2 * uosc (speed + 0.2)) cps
            , fosc 1 7 (a1 * uosc (speed - 0.15)) cps
            ]
      where
        ampSpline c = rspline (amp) (3.5 + amp) ((speed / 4) * (c - 0.1)) ((speed / 4) * (c + 0.1))

{- |
> nightPad fadeInTime cps
-}
impNightPad :: D -> Sig -> Sig
impNightPad dt = (fadeIn dt *) . stringPad 1

{- |

> stringPad amplitude cps
-}
stringPad :: Sig -> Sig -> Sig
stringPad amp cps = blp (900 + amp * 300) $ chorusPitch 3 0.1 f cps
  where
    f x = poscil 1 x giwave

giwave :: Tab
giwave = sines [1, 0.5, 0.33, 0.25, 0.0, 0.1, 0.1, 0.1]

fx1 :: Sig -> (a -> a) -> [GenFxSpec a]
fx1 dw f = [return $ FxSpec dw (return . f)]

-- | The magic cave reverb (stereo).
magicCave2 :: Sig2 -> Sig2
magicCave2 = rever2 0.99

{- | Mono reverb (based on reverbsc)

> rever2 feedback (asigLeft, asigRight)
-}
rever2 :: Feedback -> Sig2 -> Sig2
rever2 fbk (a1, a2) = (a1 + wa1, a2 + wa2)
  where
    (wa1, wa2) = reverbsc a1 a2 fbk 12000

type Feedback = Sig

------------------------------------------------------
-- wave shaper

{- | Wave shaper. The signal should be bipolar. It ranges within the interval [-1, 1].

> wshaper table amount asig

wave shaper transforms the input signal with the table.
The amount of transformation scales the signal from 0 to 1.
the amount is ratio of scaling. It expects the values from the interval [0, 1].
-}
wshaper :: Tab -> Sig -> Sig -> Sig
wshaper t amt asig = tablei (10 * amt * asig / 20) t `withDs` [1, 0.5]

{- | Wave shaper with sigmoid.

> genSaturator sigmoidRadius amount asig

* sigmoid radius is 5 to 100.

* amount is [0, 1]
-}
genSaturator :: Double -> Sig -> Sig -> Sig
genSaturator rad amt = wshaper (tanhSigmoid rad) amt

{- | Alias for

> genSaturator 5
-}
mildSaturator :: Sig -> Sig -> Sig
mildSaturator = genSaturator 1

{- | Alias for

> genSaturator 10
-}
saturator :: Sig -> Sig -> Sig
saturator = genSaturator 1.5

{- | Alias for

> genSaturator 50
-}
hardSaturator :: Sig -> Sig -> Sig
hardSaturator = genSaturator 3.5

{- | Alias for

> genSaturator 100
-}
hardSaturator2 :: Sig -> Sig -> Sig
hardSaturator2 = genSaturator 6.5

-----------------------------------------
-- attack detection

{- | Detects attacks in the signal. Outputs event stream of attack events.

> attackTrig threshold sigIn

threshhold cnmtrols the sensitivity of attack detection.
Try out different values in [0, 0.2]
-}
attackTrig :: Sig -> Sig -> SE (Evt Unit)
attackTrig thresh ain = fmap sigToEvt $ attackTrigSig thresh ain

{- | Detects attacks in the signal. Outputs trigger-signal
where 1 is when attack happens and 0 otherwise.
-}
attackTrigSig :: Sig -> Sig -> SE Sig
attackTrigSig thresh a = do
  kTime <- newCtrlRef (iWait + 1)
  kTrig <- newCtrlRef 0

  writeRef kTrig 0
  timeVal <- readRef kTime

  when1 (da >* thresh &&* timeVal >* iWait) $ do
    printk 0 timeVal
    writeRef kTrig 1
    writeRef kTime 0

  modifyRef kTime (+ 1)

  readRef kTrig
  where
    da = diffSig 0.01 $ rms a
    iWait :: Sig
    iWait = 100

    diffSig :: D -> Sig -> Sig
    diffSig dt x = x - delaySig dt x

------------------------------
-- ambient envelope

{- | Make smooth attacks for the input guitar or piano-like sounds.
It simulates automatic volume control pedal. It detects striken attacks
and makes them sound more PAD-like.

first argument is threshold that controls sensitivity of attack detection.
The values like ~ 0.01, 0.02 is good start.

To use it with guitar it's good to add a bit of distortion and compression
to the signal to make it brighter and then to apply several delays to prolong the sustain
and release phases. And it's good to add some reverb at the end.

Here is an example of how to make ambient guitar with this effect:

> main = dac proc
>
> -- | Let's assume we read guitar from the first input of sound card
> proc :: Sig2 -> SE Sig2
> proc (x, _) = mul 1 $ hall 0.25 $
>          fmap ( fromMono . (adele 0.35 1 0.65 0.34) . (adele 0.35 0.25 0.75 0.34) .
>                 saturator 0.3  . tort 0.1 0.25 .
>                 magnus 2 0.25 1.1 0.6 0.4 1.8) $
>           ambiEnv 0.02 x
>

So I've added a tape echo with magnus,
then goes combo of saturator (compressor) and tort (distortion)
then goes a couple of delays (adele)
and the last part is sweet reverb (hall).
-}
ambiEnv :: Sig -> Sig -> SE Sig
ambiEnv thresh ain = do
  attacks <- attackTrigSig thresh ain
  return $ delaySig 0.1 (env attacks) * ain
  where
    env tr = adsr140 (rms ain - 0.05) tr 4 1 1 3
