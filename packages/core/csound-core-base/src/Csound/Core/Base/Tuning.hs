module Csound.Core.Base.Tuning(
    -- * Temperament
    Temp(..), genTemp, genTempRatio,
    tempC, tempRatioC, stdTemp, stdTempRatio, barTemp, barTempRatio, concertA, ratioConcertA,

    -- * Specific temperaments
    equal1, just1, meantone, pythagor,
    werckmeister, young1, young2, young3,

    -- ** In cents
    equalCents1, justCents1, meantoneCents, pythagorCents,
    werckmeisterCents, youngCents1, youngCents2, youngCents3,

    -- * List of temperaments
    TempList(..), tempList, fromTempList, fromTempListD,

    -- * Utility functions
    cent2ratio, ratio2cent
) where

import Data.Default
import Csound.Core.Types

-- | Creates a temperament. Arguments are
--
-- > genTemp interval baseHz baseMidiPitch cents
--
-- For example:
--
-- > genTemp 2 261.63 60 [0, 100, 200 .. more cents .. , 1200]
--
-- Cent list should include the first note from the next octave(interval of temperament repetition).
genTemp :: Double -> Double -> Double -> [Double] -> Temp
genTemp tempInterval tempBase tempKey tempCents = genTempRatio tempInterval tempBase tempKey (fmap cent2ratio tempCents)

-- | Creates a temperament. Arguments are
--
-- > genTempCent interval baseHz baseMidiPitch ratios
--
-- For example:
--
-- > genTempRatio 2 261.63 60 [1, .. more ratios .. , 2]
--
-- Cent list should include the first note from the next octave(interval of temperament repetition).
genTempRatio :: Double -> Double -> Double -> [Double] -> Temp
genTempRatio tempInterval tempBase tempKey tempRatios = Temp $ doubles vals
    where vals = [fromIntegral $ (length tempRatios) - 1, tempInterval, tempBase, tempKey] ++ tempRatios

-- | Temperament with base note at note C (261.63 Hz) and an octave as interval (2).
-- The argument is the list of ratios.
tempRatioC :: [Double] -> Temp
tempRatioC = genTempRatio 2 261.63 60

-- | Temperament with base note at note C (261.63 Hz) and an octave as interval (2).
-- The argument is the list of cents.
tempC :: [Double] -> Temp
tempC = genTemp 2 261.63 60

-- | Temperament with 9th note tuned to 440 Hz (Concert A).
-- The argument is the list of ratios.
stdTempRatio :: [Double] -> Temp
stdTempRatio  = ratioConcertA 440

-- | Temperament with 9th note tuned to 440 Hz (Concert A).
-- The argument is the list of cents.
stdTemp :: [Double] -> Temp
stdTemp = concertA 440

-- | Baroque Temperament with 9th note tuned to 415 Hz (Concert A).
-- The argument is the list of ratios.
barTempRatio :: [Double] -> Temp
barTempRatio  = ratioConcertA 415

-- | Baroque Temperament with 9th note tuned to 415 Hz (Concert A).
-- The argument is the list of cents.
barTemp :: [Double] -> Temp
barTemp = concertA 415

-- | Temperament with 9th note tuned to 440 Hz (Concert A).
-- The argument is the list of ratios.
ratioConcertA :: Double -> [Double] -> Temp
ratioConcertA hz ratios = genTempRatio 2 (hz / (ratios !! 9)) 60 ratios

-- | Temperament with 9th note tuned to 440 Hz (Concert A).
-- The argument is the list of cents.
concertA :: Double -> [Double] -> Temp
concertA hz cents = ratioConcertA hz (fmap cent2ratio cents)


-- | Data structure for musical temperament.
-- The value can be created with constructors @genTemp@ and @genTempCent@.
-- It can be passed as an argument to the instrument (it can be a part of the note).
newtype Temp = Temp { unTemp :: Tab }
  deriving newtype (Val, FromTuple, Tuple, Arg)

instance Default Temp where
    def = equal1

-- | List of temperaments (or more precisely f-table of temperaments).
-- It can be passed as an argument to the instrument (it can be a part of the note).
newtype TempList = TempList { unTempList :: TabList }
  deriving newtype (Val, FromTuple, Tuple, Arg)

-- | Creates a list of temperaments.
tempList :: [Temp] -> TempList
tempList xs = TempList $ tabList $ fmap unTemp xs

-- | Selects one of the temperaments by index.
fromTempList :: TempList -> Sig -> Temp
fromTempList (TempList tab) asig = Temp $ fromTabList tab asig

-- | Selects one of the temperaments by index. Works at the time of instrument initialization (remains constant).
fromTempListD :: TempList -> D -> Temp
fromTempListD (TempList tab) a = Temp $ fromTabListD tab a

-- | Converts cents to ratios.
cent2ratio :: Floating a => a -> a
cent2ratio x = 2 ** (x / 1200)

-- | Converts ratios to cents.
ratio2cent :: Floating a => a -> a
ratio2cent x = 1200 * logBase 2 x

equalCents1, justCents1, meantoneCents, pythagorCents, werckmeisterCents,
  youngCents1, youngCents2, youngCents3 :: [Double]

equalCents1         = fmap (* 100) [0 .. 12]
justCents1          = fmap ratio2cent [1/1, 16/15,   9/8, 6/5, 5/4, 4/3, 45/32,   3/2, 8/5, 5/3, 9/5, 15/8,  2/1]
meantoneCents       = [0,    76.0,    193.2,   310.3,   386.3,   503.4,   579.5,   696.8,   772.6,   889.7,   1006.8,  1082.9,  1200]
pythagorCents       = [0,   113.7,   203.9,   294.1,   407.8,   498, 611.7,   702, 792.2,   905.9,   996.1,   1109.8, 1200]
werckmeisterCents   = [0,  90.225,  192.18,  294.135, 390.225, 498.045, 588.27,  696.09,  792.18,  888.27,  996.09,  1092.18, 1200]

youngCents1         = [0,    93.9,    195.8,   297.8,   391.7,   499.9,   591.9,   697.9,   795.8,   893.8,   999.8,   1091.8,  1200]
youngCents2         = zipWith (+) equalCents1 [0, 0.1, 2.1, 4, -2.1, 6.1, -1.8, 4.2, 2.1, 0, 6, -2, 0]
youngCents3         = zipWith (+) equalCents1 [0, -3.9, 2, 0, -2, 3.9, -5.9, 3.9, -2, 0, 2, -3.9, 0]

toTemp :: [Double] -> Temp
toTemp = tempC

-- | Equal temperament
equal1 :: Temp
equal1          = toTemp equalCents1

-- | Just intonation
just1 :: Temp
just1           = toTemp justCents1

-- | Meantone temperament
meantone :: Temp
meantone       = toTemp meantoneCents

-- | Pythagorean tuning
pythagor :: Temp
pythagor       = toTemp pythagorCents

-- | Werckmeister III temperament. Probably it was temperament of the Bach musical era.
werckmeister :: Temp
werckmeister   = toTemp werckmeisterCents

-- | Tomas Young temperament
young1 :: Temp
young1          = toTemp youngCents1

-- | Tomas Young temperament 1 (aligned with ET by C and A)
young2 :: Temp
young2         = toTemp youngCents2

-- | Tomas Young temperament 2 (aligned with ET by C and A)
young3 :: Temp
young3         = toTemp youngCents3
