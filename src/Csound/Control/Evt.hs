{-# Language TupleSections, BangPatterns, FlexibleContexts #-}
module Csound.Control.Evt(
    Evt(..), Trig, Snap, Bam,
    -- * core funs
    boolToEvt, evtToBool, sigToEvt, filterE, 
    accumE, accumSE, filterAccumSE, filterAccumE, snapshot, snaps,
    -- * aux funs
    cycleE, iterateE, repeatE, appendE, mappendE,
    oneOf, freqOneOf, freqAccum, randDs, randInts, range, listAt,   
    every        
) where

import Data.Monoid
import Data.Boolean

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Event
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Exp.GE
import Csound.Exp.Logic
import Csound.Exp.Numeric(intD)

import Csound.Render.Channel(random)

----------------------------------------------------------------------
-- higher level evt-funs

cycleE :: (CsdTuple a, Arg a) => [a] -> Evt b -> Evt a
cycleE vals evts = listAt vals $ range (0, int $ length vals) evts

listAt :: (CsdTuple a, Arg a) => [a] -> Evt D -> Evt a
listAt vals evt
    | null vals = mempty
    | otherwise = fmap (atArg vals) $ filterE within evt
    where
        within x = (x >=* 0) &&* (x <* len)
        len = int $ length vals

atArg :: (CsdTuple a, Arg a) => [a] -> D -> a
atArg as ind = guardedArg (zip (fmap (\x -> int x ==* ind) [0 .. ]) as) (head as)

range :: (D, D) -> Evt b -> Evt D
range (xMin, xMax) = iterateE xMin $ \x -> ifB (succ x >=* xMax) xMin (succ x)

randInts :: (D, D) -> Evt b -> Evt D
randInts (xMin, xMax) = accumSE (0 :: D) $ const $ \s -> fmap (, s) $ rnd
    where rnd = fmap (intD . readSnap) $ random (sig $ intD xMin) (sig $ intD xMax)

randDs :: Evt b -> Evt D
randDs = accumSE (0 :: D) $ const $ \s -> fmap (, s) $ fmap readSnap $ random 0 1 

iterateE :: (CsdTuple a) => a -> (a -> a) -> Evt b -> Evt a
iterateE s0 f = accumE s0 (const phi)
    where phi s = (s, f s)

repeatE :: CsdTuple a => a -> Evt b -> Evt a
repeatE a = fmap (const a)

appendE :: CsdTuple a => a -> (a -> a -> a) => Evt a -> Evt a
appendE empty append = accumE empty phi
    where phi a s = let s1 = s `append` a in (s1, s1)

mappendE :: (Monoid a, CsdTuple a) => Evt a -> Evt a
mappendE = appendE mempty mappend

oneOf :: (CsdTuple a, Arg a) => [a] -> Evt b -> Evt a
oneOf vals evt = listAt vals $ randInts (0, int $ length vals) evt

type Rnds a = [(D, a)]

freqOneOf :: (CsdTuple a, Arg a) => Rnds a -> Evt b -> Evt a
freqOneOf rnds evt = fmap (takeByWeight accs vals) $ randDs evt 
    where
        accs = accumWeightList $ fmap fst rnds
        vals = fmap snd rnds

takeByWeight :: (CsdTuple a, Arg a) => [D] -> [a] -> D -> a
takeByWeight accumWeights vals at = 
    guardedArg (zipWith (\w val -> (at <* w, val)) accumWeights vals) (last vals)

accumWeightList :: Num a => [a] -> [a]
accumWeightList = go 0
    where go !s xs = case xs of
            []   -> []
            a:as -> a + s : go (a + s) as
    
freqAccum :: (CsdTuple s, CsdTuple (b, s), Arg (b, s)) 
    => s -> (a -> s -> Rnds (b, s)) -> Evt a -> Evt b 
freqAccum s0 f = accumSE s0 $ \a s -> 
    let rnds = f a s
        accs = accumWeightList $ fmap fst rnds
        vals = fmap snd rnds
    in  fmap (takeByWeight accs vals . readSnap) $ random 0 1


every :: (CsdTuple a, Arg a) => Int -> [Int] -> Evt a -> Evt a
every empties beats = masked mask  
    where mask = (fmap (\x -> if x then 1 else 0) $ (replicate empties False) ++ patternToMask beats)

masked :: (CsdTuple a, Arg a) => [D] -> Evt a -> Evt a
masked ms = filterAccumE 0 $ \a s -> 
    let n  = int $ length ms
        s1 = ifB (s + 1 <* n) (s + 1) 0
    in  (atArg ms s ==* 1, a, s1)

patternToMask :: [Int] -> [Bool]
patternToMask xs = case xs of
    []   -> []
    a:as -> single a ++ patternToMask as
    where single n 
            | n <= 0    = []
            | otherwise = True : replicate (n - 1) False

