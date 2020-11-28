-- | Shortcuts for common chords.
module Csound.Sam.Chord (
	chTrans, chRot, 
    chMin, chMaj, chLead, 
    chMaj7, chMin7, ch7, chLead7, 
    atMaj, atMin, atMaj7, atMin7        
) where

import Csound.Base(int, D)
import Csound.Sam(Chord)

-- | A major chord.
chMaj :: Chord
chMaj = [0, 4, 7]

-- | A minor chord
chMin :: Chord
chMin = [0, 3, 7]

-- | A lead tone triad.
chLead :: Chord
chLead = [0, 3, 6]

-- |  A dominant seventh chord.
ch7 :: Chord
ch7 = [0, 4, 7, 10]

-- | A major seventh chord.
chMaj7 :: Chord
chMaj7 = [0, 4, 7, 11]

-- | A minor seventh chord.
chMin7 :: Chord
chMin7 = [0, 3, 7, 10]

-- | A lead tone seventh chord.
chLead7 :: Chord
chLead7 = [0, 3, 6, 10]

chTrans :: D -> Chord -> Chord
chTrans k = fmap (k + )

-- | Rotates the chord.
chRot :: Int -> Chord -> Chord
chRot m
	| m == 0    = id
	| m < 0     = rotPos m
	| otherwise = rotNeg (abs m)
	where		
		rotPos 1 xs = tail xs ++ [head xs + 12]
		rotPos n xs = rotPos (n - 1) (rotPos 1 xs)

		rotNeg 1 xs = (last xs - 12) : init xs
		rotNeg n xs = rotNeg (n - 1) (rotNeg 1 xs)
    
toneMsg :: a
toneMsg = error $ "Tone number should belong to interval (0, 6)"

toneMap :: Int -> a -> a -> a -> a -> a -> a -> a -> a
toneMap n a0 a1 a2 a3 a4 a5 a6 = case n of
        0 -> a0
        1 -> a1
        2 -> a2
        3 -> a3
        4 -> a4
        5 -> a5
        6 -> a6
        _ -> toneMsg

-- | Chord in major scale at the given note (if there are seven notes)
atMaj :: Int -> Chord
atMaj n = chTrans (int $ 12 * oct + inMaj tone) $ toneMap tone chMaj chMin chMin chMaj chMaj chMin chLead
	where (oct, tone) = octTone n

-- | Chord in minor scale at the given note (if there are seven notes)
atMin :: Int -> Chord
atMin n = chTrans (int $ 12 * oct + inMin tone) $ toneMap tone chMin chLead chMaj chMin chMin chMaj chMaj
	where (oct, tone) = octTone n

-- | Seventh chord in major scale at the given note (if there are seven notes)
atMaj7 :: Int -> Chord
atMaj7 n = chTrans (int $ 12 * oct + inMaj tone) $ toneMap tone chMaj7 chMin7 chMin7 chMaj7 ch7 chMin7 chLead7
	where (oct, tone) = octTone n

-- | Seventh chord in minor scale at the given note (if there are seven notes)
atMin7 :: Int -> Chord
atMin7 n = chTrans (int $ 12 * oct + inMin tone) $ toneMap tone chMin7 chLead7 chMaj7 chMin7 chMin7 chMaj7 ch7
	where (oct, tone) = octTone n

inMaj :: Int -> Int
inMaj x = toneMap x 0 2 4 5 7 9 11

inMin :: Int -> Int
inMin x = toneMap x 0 2 3 5 7 9 10

octTone :: Int -> (Int, Int)
octTone n 
	| n < 0     = (oct - 1, tone + 7)
	| otherwise = (oct, tone)
	where (oct, tone) = quotRem n 7

