-- | Resonators
module Csound.Catalog.Reson(
    Reson,
    -- * Vowels

    -- | Resonators for the vowel sounds.
    anO, anA, anE, anIY, anO2, wow,

    -- * Modal synthesis
    
    -- ** Instruments
    strikeModes, scrapeModes, modesInstr, wetModesInstr,

    -- ** Exciters
    strike, scrape, simpleStrike, simpleScrape,

    -- ** Parameters
    -- | Parameters for the function 'Csound.Air.modes'
    Modes(..), fromModes,
    singleModes, dahinaModes, banyanModes, xylophoneModes, tibetanBowlModes180, 
    spinelSphereModes, potLidModes, redCedarWoodPlateModes, 
    tubularBellModes, redwoodPlateModes, douglasFirWoodPlateModes,
    uniformWoodenBarModes, uniformAluminumBarModes, vibraphoneModes1, 
    vibraphoneModes2, chalandiPlatesModes, tibetanBowlModes152, 
    tibetanBowlModes140, wineGlassModes, smallHandbellModes, 
    albertClockBellBelfastModes, woodBlockModes
) where

import Csound.Base

-- | List of pairs of 
--
-- > [(centerFrequency, bandWidth)]
--
-- It's a list of parameters for a bunch of the band pass filters (like reson, or bp).
-- Reson is intended to be used with functions 'Csound.Air.resons' and 'Csound.Air.resonsBy'.
type Reson = [(Sig, Sig)]

anO, anA, anE, anIY, anO2 :: Reson

anO  = [(280, 20), (650, 25), (2200, 30), (3450, 40), (4500, 50)]
anA  = [(650, 50), (1100, 50), (2860, 50), (3300, 50), (4500, 50)] 
anE  = [(500, 50), (1750, 50), (2450, 50), (3350, 50), (5000, 50)]
anIY = [(330, 50), (2000, 50), (2800, 50), (3650, 50), (5000, 50)]
anO2 = [(400, 50), (840, 50), (2800, 50), (3250, 50), (4500, 50)]

-- | Produces a 'wow'-effect if modifier rises and then falls down.
--
-- > y = wow modifierSig x
wow :: Sig -> Reson -> Reson
wow kmod = fmap $ \(a, b) -> (a + kmod, b)

---------------------------------------------------
-- modes instruments

strikeModes :: Modes Sig -> Sig -> Sig
strikeModes ms cps = modesInstr ms (blp cps simpleStrike) cps 

scrapeModes :: Modes Sig -> Sig -> SE Sig
scrapeModes ms cps = fmap (\exciter -> modesInstr ms exciter cps) simpleScrape

modesInstr :: Modes Sig -> Sig -> Sig -> Sig
modesInstr = wetModesInstr 0.1

wetModesInstr :: D -> Modes Sig -> Sig -> Sig -> Sig
wetModesInstr dryRatio ms exciter cps
    = (modesGain ms * )
    $ dryWet (sig dryRatio) (modes (fromModes ms) cps) exciter   

---------------------------------------------------
-- exciters

simpleStrike :: Sig
simpleStrike = mpulse 1 0

strike :: D -> SE Sig
strike dt' = do
    onHighDur   <- noise 1 0
    onNormDur   <- noise (osciln 1 (1 / dt) decayShape 1) 0
        
    return $ guardedTuple 
        [ (sig dt `lessThan` 0.001,    onLowDur)
        , (sig dt >=* 1,        onHighDur)
        ]                       onNormDur        
    where
        onLowDur    = mpulse 1 0 
        decayShape = estartEnds [1, -0.002, 0]

        dt = maxB dt' 0.00001

simpleScrape  :: SE Sig
simpleScrape = scrape 5000 50

scrape :: Sig -> Sig -> SE Sig
scrape lpCps hpCps = fmap (bhp hpCps . blp lpCps) $ pinkish 0.005

---------------------------------------------------

data Modes a = Modes
    { modesFrequencies      :: [a]
    , modesQualityFactor    :: a
    , modesQualityRatios    :: [a]
    , modesGain             :: a }

fromModes :: Num a => Modes a -> [(a, a)]
fromModes a = zip (modesFrequencies a) (fmap (modesQualityFactor a * ) $ modesQualityRatios a)

singleModes, dahinaModes, banyanModes, xylophoneModes, tibetanBowlModes180, spinelSphereModes, 
    potLidModes, redCedarWoodPlateModes, tubularBellModes, redwoodPlateModes, douglasFirWoodPlateModes, 
    uniformWoodenBarModes, uniformAluminumBarModes, vibraphoneModes1, vibraphoneModes2, chalandiPlatesModes,
    tibetanBowlModes152, tibetanBowlModes140, wineGlassModes, smallHandbellModes, albertClockBellBelfastModes, 
    woodBlockModes :: Fractional a => Modes a

singleModes = Modes 
    { modesFrequencies      = [1, 1]
    , modesQualityFactor    = 1000
    , modesQualityRatios    = repeat 1
    , modesGain             = 15 }

dahinaModes = Modes 
    { modesFrequencies      = [1, 2.89, 4.95, 6.99, 8.01, 9.02]
    , modesQualityFactor    = 50
    , modesQualityRatios    = repeat 1
    , modesGain             = 15 }

banyanModes = Modes 
    { modesFrequencies      = [1, 2.0, 3.01, 4.01, 4.69, 5.63]
    , modesQualityFactor    = 444
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

xylophoneModes = Modes
    { modesFrequencies      = [1, 3.932, 9.538,	16.688,	24.566,	31.147]
    , modesQualityFactor    = 200
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

-- (180mm)
tibetanBowlModes180 = Modes
    { modesFrequencies      = [1, 2.77828, 5.18099, 8.16289, 11.66063, 15.63801, 19.99]
    , modesQualityFactor    = 2200
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

-- with diameter of 3.6675mm
spinelSphereModes = Modes
    { modesFrequencies      = 
            [1,	1.026513174725,	1.4224916858532,	1.4478690202098,	1.4661959580455
            , 1.499452545408,	1.7891839345101,	1.8768994627782,	1.9645945254541
            , 1.9786543873113,	2.0334612432847,	2.1452852391916,	2.1561524686621
            , 2.2533435661294,	2.2905090816065,	2.3331798413917,	2.4567715528268
            , 2.4925556408289,	2.5661806088514,	2.6055768738808,	2.6692760296751
            , 2.7140956766436,	2.7543617293425,	2.7710411870043 
            ]
    , modesQualityFactor    = 80
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }
    
potLidModes = Modes
    { modesFrequencies      = [ 1, 3.2, 6.23, 6.27, 9.92, 14.15 ]
    , modesQualityFactor    = 1500
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

redCedarWoodPlateModes = Modes
    { modesFrequencies      = [ 1, 1.47, 2.09, 2.56 ]
    , modesQualityFactor    = 100
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }
    
tubularBellModes = Modes
    { modesFrequencies      = [ 272/437, 538/437, 874/437, 1281/437, 1755/437, 2264/437, 2813/437, 3389/437, 4822/437, 5255/437 ]
    , modesQualityFactor    = 2000
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

redwoodPlateModes = Modes
    { modesFrequencies      = [ 1, 1.47, 2.11, 2.57 ]
    , modesQualityFactor    = 100
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

douglasFirWoodPlateModes = Modes
    { modesFrequencies      = [ 1, 1.42, 2.11, 2.47 ]
    , modesQualityFactor    = 100
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }
    
uniformWoodenBarModes = Modes
    { modesFrequencies      = [ 1, 2.572, 4.644, 6.984, 9.723, 12 ]
    , modesQualityFactor    = 300
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

uniformAluminumBarModes = Modes
    { modesFrequencies      = [1, 2.756, 5.423, 8.988, 13.448, 18.680 ]
    , modesQualityFactor    = 1000
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

vibraphoneModes1 = Modes 
    { modesFrequencies      = [1, 3.984, 10.668, 17.979, 23.679, 33.642]
    , modesQualityFactor    = 2000
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }
    
vibraphoneModes2 = Modes 
    { modesFrequencies      = [1, 3.997, 9.469, 15.566, 20.863, 29.440]
    , modesQualityFactor    = 2000
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

chalandiPlatesModes = Modes 
    { modesFrequencies      = [1, 1.72581, 5.80645, 7.41935, 13.91935]
    , modesQualityFactor    = 500
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

tibetanBowlModes152 = Modes 
    { modesFrequencies      = [1, 2.66242, 4.83757, 7.51592, 10.64012, 14.21019, 18.14027]
    , modesQualityFactor    = 2200
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

tibetanBowlModes140 = Modes 
    { modesFrequencies      = [1, 2.76515, 5.12121, 7.80681, 10.78409]
    , modesQualityFactor    = 2200
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

wineGlassModes = Modes 
    { modesFrequencies      = [1, 2.32, 4.25, 6.63, 9.38]
    , modesQualityFactor    = 1500
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

smallHandbellModes = Modes 
    { modesFrequencies      =
        [ 1, 1.0019054878049, 1.7936737804878, 1.8009908536585, 2.5201981707317
        , 2.5224085365854, 2.9907012195122, 2.9940548780488, 3.7855182926829
        , 3.8061737804878, 4.5689024390244, 4.5754573170732, 5.0296493902439
        , 5.0455030487805, 6.0759908536585, 5.9094512195122, 6.4124237804878
        , 6.4430640243902, 7.0826219512195, 7.0923780487805, 7.3188262195122
        , 7.5551829268293 
        ]
    , modesQualityFactor    = 2500
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

albertClockBellBelfastModes = Modes 
    { modesFrequencies      =
            [ 2.043260,1.482916,1.000000,3.328848,4.761811,1.477056,0.612007
            ,2.661295,1.002793,4.023776,0.254139,2.043916,4.032463,2.659438
            ,4.775560,5.500494,3.331014,0.809697,2.391301, 0.254098,1.901476,2.366563 
            ]
             -- ,0.614968,2.046543,1.814887,3.130744,2.484426,0.558874,0.801697,0.070870,3.617036,2.782656
    , modesQualityFactor    = 2400
    , modesQualityRatios     = repeat 1
    , modesGain             = 15 }

woodBlockModes = Modes 
    { modesFrequencies      = [915/915,1540/915,1863/915,3112/915]
    , modesQualityFactor    = 60
    , modesQualityRatios    = repeat 1
    , modesGain             = 15 }

