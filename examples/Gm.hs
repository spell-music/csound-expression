{-# Language DeriveFunctor #-}
-- | Accords in just intonation Gm with 14/8 beat.
module Main where

import Csound.Base

import Color(marimbaSynth)

instr (amp, cps) = sig amp * env * osc (sig cps)
    where
        env = once $ lins [0.00001, 30, 1, 50, 0.5, 100, 0.00001]

en :: Sco a -> Sco a
en = str (1/8)

-- volumes
baseVolume = 0.35
v3 = baseVolume * 0.8
v2 = baseVolume * 0.7
v1 = baseVolume * 0.5

-- pitches (Gm + Ab)
baseTone = 391.995   -- G
-- phrygian scale: sTTTsTT 
tones = [1, 16/15, 6/5, 4/3, 3/2, 8/5, 9/5]        

cpsMap f x = fmap phi x
    where phi (amp, cps) = (amp, f cps)

lo1, hi1 :: Functor f => f (a, Int) -> f (a, Int) 
lo1 = cpsMap (\x -> x - 7)
hi1 = cpsMap (\x -> x + 7)

trip2cps = cpsMap t2cps

t2cps :: Int -> D
t2cps n = double $ (2 ^^ oct) * baseTone * tones !! step
    where (oct, step) = divMod n 7

data Trip a = Trip
    { lead :: [a]
    , body :: (a, a) 
    } deriving (Functor)

-- single lead
trip lead = Trip (repeat lead)

-- main left/right hand pattern

-- the first accent is a bit louder than other ones. v3 vs v2
rhp :: Trip a -> Sco (D, a)
rhp (Trip leads (a, b)) = en $ mel $ zipWith ($) [triple v3, triple v2, quatra, doub, doub] leads
    where
        triple acc lead = firstLead acc [lead, a, b]  
        quatra     lead = firstLead v3  [lead, a, b, a]
        doub       lead = firstLead v2  [lead, a]

lhp :: Trip a -> Sco (D, a)
lhp (Trip leads (a, b)) = en $ mel $ zipWith ($) [triple v3, triple v2, quatra, quatra]  leads
    where
        triple acc lead = firstLead acc [lead, a, b]  
        quatra     lead = firstLead v2  [lead, a, b, a]

firstLead :: D -> [a] -> Sco (D, a)
firstLead accVol (a:as) = melMap temp $ (accVol, a) : fmap (\x -> (v1, x)) as 

bass = fmap (\x -> x - 7)

-- First part
tr = trip 4 (2, 0)         -- left tonic
tl = bass $ trip 0 (2, 4)  -- right tonic

t = (0, 2, 4)

firstPart = loopBy 2 $ har [leftHand1, rightHand1]

rightHand1 = melMap rhp' 
        [ t
        , (0, 2, 5)
        , (0, 1, 5)
        , t
        , t
        , (1, 2, 3)
        , (0, 2, 3)
        , t]
        
rhp' (a, b, c) = rhp $ trip c (b, a)

leftHand1 = melMap lhp' 
        [ (0, 2, 3) 
        , (0, 2, 5)
        , (1, 2, 5)
        , t
        , (0, 2, 3)
        , (1, 2, 3)
        , (-2, 1, 3)
        , t]
        
lhp' (a, b, c) = lhp $ bass $ trip a (b, c)

-- Second part

secondPart = loopBy 2 $ har [leftHand2, rightHand2] 

leftHand2 = loopBy 2 $ melMap lhp' 
        [ (-2, 1, 2)
        , (-3, 1, 2)]

rightHand2 = mel 
    [ t, e, t, s ]
    where p a b (star1, star2) = rhp $ Trip [a, a, b, star1, star2] (2, 0)
          t = p 4 4 (4, 4)   
          e = p 4 5 (4, 4)
          s = p 4 4 (9, 7)  
        
-- all

tot = mel [loopBy 2 firstPart, secondPart, firstPart]

res = str 1.7 $ sco (onCps marimbaSynth) $ sustain 0.35 $ loopBy 4 $ trip2cps $ tot

main = dac $ mix res
