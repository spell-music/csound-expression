module WindAlg(
    WindSpec(..), RangeSpec(..), HarmSpec(..), AmpSpec(..), WaveSpec,
    fromSpec, byFreq,
    wind                 
) where

import Data.List (transpose, intersperse)
import Control.Monad.Trans.State

import Csound.Base

----------------------------------------------------------------
-- Deterministic random numbers

newtype Rnd a = Rnd { unRnd :: State D a }

instance Functor Rnd where
    fmap f (Rnd a) = Rnd $ fmap f a

instance Monad Rnd where
    return = Rnd . return
    (Rnd a) >>= f = Rnd $ a >>= unRnd . f 

evalRnd :: Rnd a -> D -> a
evalRnd = evalState . unRnd 

rndNext :: Rnd ()
rndNext = Rnd $ modify $ fracD . (* 105.947) 

rndGet :: Rnd D
rndGet = Rnd $ get

rndWithin :: (D, D) -> Rnd D
rndWithin (imin, imax) = do
    seed <- rndGet
    rndNext
    return $ imin + (imax - imin) * seed

----------------------------------------------------------------
-- tools

randiPct :: D -> D -> Sig -> Rnd Sig
randiPct pct cps asig = do
    seed <- rndWithin (0, 1)
    return $ asig * (1 + (randi (sig pct) (sig cps) `withSeed` seed))

minDt :: D -> D -> D 
minDt n x = maxB x (n / kr)
    where kr = getSampleRate / getBlockSize

----------------------------------------------------------------
-- algorithm parameters

-- | Winds algorithm specification.
data WindSpec = WindSpec
    { windRange         :: (D, D, D) -> D -> ([(Sig, Tab)], D)
    , windVibratoDur    :: (D, D, D) -> D
    , windFreqDeviation :: ((D, D), (D, D), (D, D), (D, D)) }

-- | Harmonics per pitch range.
data RangeSpec = RangeSpec 
    { rangeFreq     :: D
    , rangeNorm     :: D
    , rangeHarms    :: [HarmSpec] }

-- | The harmonics.
data HarmSpec = HarmSpec 
    { harmAmp   :: AmpSpec
    , harmWave  :: WaveSpec }

-- | Envelopes for linseg
data AmpSpec = AmpSpec     
    { ampAttack     :: [D]      
    , ampSustain    :: [D]
    , ampDecay      :: [D] }

-- | Not scaled sine harmonics.
type WaveSpec = [Double]

----------------------------------------------------------------
-- The algorithm

wind :: WindSpec -> D -> D -> D -> D -> D -> D -> D -> Sig
wind spec seed cps vibPercent attack sustain decay brightnessLevel = 
    evalRnd (rndWind spec cps vibPercent attack sustain decay brightnessLevel) seed

rndWind :: WindSpec -> D -> D -> D -> D -> D -> D -> Rnd Sig
rndWind spec cps vibCoeff attack sustain decay brightnessLevel = do
    iphase  <- rndWithin (0, 1) 
    durs    <- initDurations 
    kfreq   <- vibrato (windVibratoDur spec durs) =<< (freqDeviation (windFreqDeviation spec) durs $ sig cps)

    let (harms, inorm) = windRange spec durs cps   
    harms <- mapM (ampVarOnHarm 0.02) harms    
    return $ brightness durs brightnessLevel $ sumHarms harms inorm iphase kfreq
    where
        initDurations :: Rnd (D, D, D)
        initDurations = do
            iattack <- rndWithin (attack * 0.9, attack * 1.1)
            idecay  <- rndWithin (decay  * 0.9, decay  * 1.1)
            return (minDt 6 iattack, minDt 5 sustain, minDt 6 idecay)

        vibrato :: D -> Sig -> Rnd Sig
        vibrato xdur asig = do   
            let ivibdepth = abs (vibCoeff * cps)
            kvibdepth <- randiPct 0.1 5 $ sig ivibdepth * linseg [0.1, 0.8 * xdur, 1, 0.2 * xdur, 0.7]
            [ivibr1, ivibr2, ivibr3] <- mapM rndWithin $ replicate 3 (0, 1)
            kvibrate <- randiPct 0.1 5 $ 
                ifB (sig vibCoeff >* 0) 
                    -- if vibrato is positive it gets faster
                    (linseg [2.5 + ivibr1, xdur, 4.5 + ivibr2])
                    -- if vibrato is negative it gets slower
                    (linseg [3.5 + ivibr1, 0.1, 4.5 + ivibr2, xdur - 0.1, 2.5 + ivibr3])
            return $ asig + kvibdepth * osc kvibrate

        freqDeviation :: ((D, D), (D, D), (D, D), (D, D)) -> (D, D, D) -> Sig -> Rnd Sig
        freqDeviation (f1, f2, f3, f4) (iattack, isustain, idecay) asig = do
            [fdev1, fdev2, fdev3, fdev4] <- mapM rndWithin [f1, f2, f3, f4] 
            return $ asig * (1 + linseg [fdev1, iattack, fdev2, isustain, fdev3, idecay, fdev4])

        ampVar :: D -> Sig -> Rnd Sig
        ampVar pct = randiPct pct 10

        ampVarOnHarm :: D -> (Sig, Tab) -> Rnd (Sig, Tab)
        ampVarOnHarm perct (amp, wt) = fmap (\x -> (x, wt)) $ ampVar perct amp

        sumHarms :: [(Sig, Tab)] -> D -> D -> Sig -> Sig
        sumHarms hs norm iphase kfreq = ( / sig norm) $ mean $ 
            fmap (\(amp, wt) -> oscili amp kfreq wt `withD` iphase) hs 

        brightness :: (D, D, D) -> D -> Sig -> Sig
        brightness (iattack, isustain, idecay) level asig = balance (tone asig env) asig 
            where ifiltcut = tableiD level (skipNorm $ doubles [40, 40, 80, 160, 320, 640, 1280, 2560, 5120, 10240, 10240])
                  env = linseg [0, iattack, ifiltcut, isustain, ifiltcut, idecay, 0]  

----------------------------------------------------------------
-- Converting specification to signals

fromSpec :: [RangeSpec] -> (D, D, D) -> D -> ([(Sig, Tab)], D)
fromSpec specs durs ifreq = (hs, inorm)
    where 
        inorm = byFreq ifreq $ fmap (\x -> (rangeFreq x, rangeNorm x)) specs
        hs    = fmap (byFreq ifreq . zip freqs . fmap (fromHarmSpec durs)) $ transpose $ fmap rangeHarms specs

        freqs = fmap rangeFreq specs

byFreq :: CsdTuple a => D -> [(D, a)] -> a
byFreq ifreq as = guardedTuple (fmap (\(cps, val) -> (sig ifreq <* sig cps, val)) $ init as) (snd $ last as)


fromHarmSpec :: (D, D, D) -> HarmSpec -> (Sig, Tab)
fromHarmSpec durs spec = (fromAmpSpec durs $ harmAmp spec, fromWaveSpec $ harmWave spec)

fromAmpSpec :: (D, D, D) -> AmpSpec -> Sig
fromAmpSpec (attack, sustain, decay) spec = linseg $ att ++ (tail sus) ++ (tail dec)
    where phi dt select = intersperse (dt / fromIntegral (pred $ length $ select spec)) (select spec)
          att = phi attack  ampAttack  
          sus = phi sustain ampSustain
          dec = phi decay   ampDecay

fromWaveSpec :: WaveSpec -> Tab
fromWaveSpec = skipNorm . sines


