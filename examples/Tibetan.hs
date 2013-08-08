{- | additional parameters: ioff, irise, idec

This remarkable tibetan harmonic chant like effect is created by nine sinusoidal 
oscillators, whose frequencies are almost identical: separated by a fraction of 
1 Hz from each other. Thus for each component, amplitude modulation leads to its 
enhancement or cancelling out in turn. In his composition 'Mutations', Risset 
gives the instrument two different envelopes: one with sharp rise and one is a 
more gradual rise.
-}
module Tibetan where

import Data.List(zip4)
import Data.Traversable(traverse)

import Control.Monad.Trans.State
import Control.Applicative hiding ((<*))

import System.Random

import Csound 
import Color(blurp, blue)

-- | A pure tibetan instrument with randomized parameters.
tibetan :: (D, D, D, D) -> SE Sig
tibetan (amp, cps, rise, dec) = fmap (\x -> pureTibetan (amp, cps, x, rise, dec)) off
    where off = fmap (\x -> 0.3 + ir x) $ rand 0.15

pureTibetan :: (D, D, D, D, D) -> Sig
pureTibetan (amp, cps, off, rise, dec) = mean $ fmap partial $ 0 : offs ++ (fmap negate offs)
    where offs = [1 .. 4]
          partial rat = linen (sig amp) rise idur dec * oscBy wave (sig $ cps + off * rat)   
          wave = ifB (cps <* 230) (waveBy 5) (ifB (cps <* 350) (waveBy 3) (waveBy 1))
          waveBy n = sines $ [0.3, 0, 0, 0] ++ replicate n 0.1

-----------------------------------------------------------
-- scores

-- tibetan

instant = 1
med = 3
long = 5

-- intersection
deep = 50
shallow = 90
sym = 70

-- myxo mode
ts = [1, 9/8, 5/4, 4/3, 3/2, 5/3, 16/9]

id2cps oct n = baseTone * (2 ^^ (oct + curOct)) * ts !! curId
    where (curOct, curId) = divMod n 7

data Act = Tone Int | Repeat Int | Wait Double

r = Repeat
w = Wait

instance Num Act where    
    fromInteger = Tone . fromInteger

type N = (Double, Double, Iamp, Icps, D, D)


data St = St 
    { stRepeat :: Int
    , stWait   :: Double
    , stSpan   :: (Double, Double)    
    , stRnds   :: [Double] }

-- constants
baseTone = 110 -- an A
innerOverlap = 0.5
noteLength = 25
fixedNoteLength = 15
offset = 0.03
rise = 5
dec = 15
durStep = 7

initRepeat = 2
initWait = 0.7
initSpan = (0, 0)

updateSt f = modify f >> return []

turtle :: Act -> State St [N]
turtle x = case x of
    Repeat n -> updateSt $ \s -> s{ stRepeat = n }
    Wait n -> updateSt $ \s -> s{ stWait = n } 
    -- tones
    Tone t -> state (getNotes t)

getNotes :: Int -> St -> ([N], St)
getNotes k st = (notes, st') 
    where t0s = startTimes (offsetStartTime (stWait st) $ stSpan st) durStep
          dts = durs noteLength
          cpss = freqs (ts !! (k - 1))
          amps = fmap cps2amp cpss
          offs = offsets offset
          decs = decays dec
          riss = rises rise

          notes = take (stRepeat st) $ getZipList $ (\t0 dt amp cps ris dec -> 
                (t0, dt, double amp, double cps, double ris, double dec)) <$>
                ZipList t0s <*> ZipList dts <*> ZipList amps <*> ZipList cpss <*> ZipList decs <*> ZipList riss

          st' = st{ stSpan = getSpan (stRepeat st) t0s dts }

offsetStartTime :: Double -> (Double, Double) -> Double
offsetStartTime k (t0, dt) = t0 + k * dt

startTimes :: Double -> Double -> [Double]
startTimes t0 step = fmap (+ t0) $ [0, step ..]

durs    = repeat
offsets = repeat
rises   = repeat
decays  = repeat

getSpan :: Int -> [Double] -> [Double] -> (Double, Double)
getSpan num starts durs = (starts !! n, durs !! n)
    where n = num - 1

freqs k = fmap (* base) octs
    where octs = cycle [1, 0.5, 2, 1, 2, 1, 1, 0.5]
          base = k * baseTone  

cps2amp a = 0.5 * full a
    where full x
            | x < 100 = 1
            | x < 200 = 0.8
            | x < 300 = 0.6
            | x < 400 = 0.5
            | otherwise = 0.4

run :: [Act] -> IO [N]
run as = fmap concat $ fmap (evalState (mapM turtle as)) initSt
    where initSt = fmap (St initRepeat initWait initSpan) $ fmap (randomRs (0, 1)) newStdGen
   
acts :: [Act]
acts = concat $ replicate 2 $ [1, 1, 5, 2, 5, 7, 5, 1, 1, 3, 5, 3, 1, 1, 1, 5, 1, 5, 6, 3, 2, 1, 1, 5, 1, 6, 4, 5, 1, 1]

    
note (start, dur, amp, cps, rise, dec) = delay start $ stretch dur $ 
                temp (amp, cps, rise, dec)

globalEffect =  mix (effect $ blp 5000 . (0.3 * )) . mix rever

res2 ns = chord [res ns, delay 13 $ res ns] 

res ns = sco tibetan $ chord $ fmap note ns

rever x = do
    _ <- delayr 3
    [aleft, aright] <- mapM deltap3 [2.50, 1.25]
    delayw x
    return (f aleft, f aright)
    where x1 = reverb2 (0.6 * x) 6 0.5
          f a = 0.5 * (x1 + a)


------------------------------------------------------------
-- blurp

blurpVol = 0.5
introDur = 80

toStereo = mix $ \x -> (x, x)

introBlurp = toStereo $ sco blurp $ introDur *| temp (0.7 * blurpVol)

blurpSco = toStereo $ line [rest 100, cone 15 0.7, rest 120, cone 10 0.4]
    where cone dt v = mix (\x -> linen x (0.25 * idur) idur (0.25 * idur)) $ sco blurp $ dt *| temp (v * blurpVol)

------------------------------------------------------------
-- stars

starLength      = 2.5 * 60
starParams      = zip4 starVolumes starLfos starHarms starSweeps

starVolume      = 0.3
starVolumes     = fmap (starVolume * ) $ cycle [0.7, 0.3, 0.5, 0.8, 0.9, 0.5, 0.2]
starLfos        = cycle [23, 10, 5, 15, 17]
starHarms       = cycle [10, 6, 7, 10]
starSweeps      = cycle [0.7, 0.6, 0.6, 0.5, 0.8, 0.9, 0.25, 0.7, 0.5, 0.5, 0.6]

starTotalDelay  = 5 * 60
starInitDelays  =              [ 0, 11,  2, 8,  21, 25]
starPeriods     = fmap (* 2.5) [ 7, 23, 77, 13, 17, 31]

starChord       = [0, 1, 2, 4, 7]

starSco = sco blue $
    flip evalState starParams $ traverse addParam $ 
    takeS starLength $ chord $ zipWith3 phi starInitDelays starPeriods starChord
    where phi del period note = delay del $ loop 200 $ chord [4 *| temp (double $ id2cps 2 note), rest period] 

          addParam cps = state $ \((amp, lfo, harm, sweep) : params) -> 
            ((amp, cps, lfo, harm, sweep), params) 
             
------------------------------------------------------------

main = do
    notes <- run acts
    dac $ runMix $ chord 
        [ introBlurp
        , delay (introDur * 0.70) $ chord 
            [ globalEffect $ chord 
                [res2 notes
                , delay starTotalDelay $ starSco
                , delay (3 * starTotalDelay) $ starSco]
            , blurpSco
            ]]
                
