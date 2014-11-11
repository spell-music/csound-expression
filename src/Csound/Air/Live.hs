{-# Language TypeSynonymInstances, FlexibleInstances #-}
-- | UIs for live performances
module Csound.Air.Live (
    -- * Mixer
    mixer, hmixer, mixMono,

    -- * Effects
    FxFun, FxUI(..), fxBox,
    fxColor, fxVer, fxHor, fxSca, fxApp,

    -- ** Fx units
    uiDistort, uiChorus, uiFlanger, uiPhaser, uiDelay, uiEcho,
    uiFilter, uiReverb, uiGain, uiWhite, uiPink, uiFx, uiRoom,
    uiHall, uiCave, uiSig, uiMix,

     -- * Static widgets
    AdsrBound(..), AdsrInit(..),
    linAdsr, expAdsr, 
    classicWaves,
    masterVolume, masterVolumeKnob
) where

import Control.Monad

import Data.Colour
import Data.Boolean
import qualified Data.Colour.Names as C

import Csound.Typed
import Csound.Typed.Gui
import Csound.Control.Gui(funnyRadio, mapSource)
import Csound.Typed.Opcode hiding (space)
import Csound.Types(Sig2)
import Csound.SigSpace
import Csound.Air.Wave
import Csound.Air.Fx
import Csound.Air.Misc

----------------------------------------------------------------------
-- mixer

-- | The stereo signal processing function.
type FxFun = Sig2 -> SE Sig2

-- | Widget that represents a mixer.
mixer :: [(String, SE Sig2)] -> Source Sig2
mixer = genMixer (ver, hor)

-- | Widget that represents a mixer with horizontal grouping of elements.
hmixer :: [(String, SE Sig2)] -> Source Sig2
hmixer = genMixer (hor, ver)

genMixer :: ([Gui] -> Gui, [Gui] -> Gui) -> [(String, SE Sig2)] -> Source Sig2
genMixer (parentGui, childGui) as = source $ do
    gTags <- mapM box names
    (gs, vols) <- fmap unzip $ mapM (const $ defSlider "") names
    (gMutes, mutes) <- fmap unzip $ mapM (const $ toggleSig "" False) names

    gMasterTag <- box "master"
    (gMaster, masterVol) <- defSlider ""
    (gMasterMute, masterMute) <- toggleSig "" False 
    let g = parentGui $ zipWith3 (\tag slid mute -> childGui [sca 0.8 tag, sca 8 slid, sca 1.1 mute]) 
                        (gMasterTag : gTags) (gMaster : gs) (gMasterMute : gMutes)
        muteVols = zipWith appMute mutes vols
        masterMuteVol = appMute masterMute masterVol
    res <- mul masterMuteVol $ mean $ zipWith mul muteVols sigs
    return (g, res)
    where 
        (names, sigs) = unzip as
        appMute mute vol = (port (1 - mute) 0.05) * vol

-- | Transforms the mono signal to the stereo input
-- for the mixer widget.
mixMono :: String -> Sig -> (String, SE Sig2)
mixMono name asig = (name, return (asig, asig))

defSlider :: String -> Source Sig
defSlider tag = slider tag (linSpan 0 1) 0.5

----------------------------------------------------------------------
-- effects

class FxUI a where
    applyFxArgs :: a -> [Sig] -> Sig2 -> SE Sig2
    arityFx :: a -> Int

instance FxUI (Sig2 -> Sig2) where
    applyFxArgs f _ x = return $ f x
    arityFx = const 0

instance FxUI FxFun where
    applyFxArgs f _ x = f x
    arityFx = const 0

instance FxUI a => FxUI (Sig -> a) where
    applyFxArgs f (a:as) x = applyFxArgs (f a) as x
    arityFx f = 1 + arityFx (proxy f)
        where 
            proxy :: (a -> b) -> b
            proxy _ = undefined

-- | Creates a widget that represents a stereo signal processing function.
-- The parameters of the widget are updated with sliders.
-- For example let's create a simple gain widget. It can be encoded like this:
--
-- > uiGain :: Bool -> Double -> Source FxFun
-- > uiGain isOn gain = fxBox "Gain" fx isOn [("gain", gain)]
-- >    where 
-- >        fx :: Sig -> Sig2 -> Sig2
-- >        fx = mul
--
-- Let's look at the arguments of the function
--
-- > fxBox name fx isOn args
--
-- * @name@ -- is the name of the widget
--
-- * @fx@ -- is signal processing function (see the class @FxUI@). 
--
-- * @isOn@ -- whether widget in the active state
-- 
-- * @args@ -- list of initial values for arguments and names of the arguments.
--
-- It's cool to set the color of the widget with @fxColor@ function.
-- we can make our widgets much more intersting to look at.
fxBox :: FxUI a => String -> a -> Bool -> [(String, Double)] -> Source FxFun
fxBox name fx onOff args = source $ do
    (gOff0, off) <- toggleSig name onOff
    let gOff = setFontSize 25 gOff0
    offRef <- newGlobalSERef (0 :: Sig)
    writeSERef offRef off
    let (names, initVals) = unzip $ take (arityFx fx) args  
    (gs, as)  <- fmap unzip $ mapM (\(name, initVal) -> slider name (linSpan 0 1) initVal) $ zip names initVals 
    let f x = do
        ref <- newSERef (0 :: Sig, 0 :: Sig)
        goff <- readSERef offRef
        writeSERef ref x        
        when1 (goff ==* 1) $ do
            x2 <- readSERef ref
            writeSERef ref =<< applyFxArgs fx as x2
        res <- readSERef ref        
        return res  
    let gui = setBorder UpBoxBorder $ go (length names) gOff gs
    return (gui, f)
    where 
        go n gOff gs
            | n == 0 = gOff
            | n < 4  = f (gs ++ replicate (4 - n) space)
            | otherwise = f gs
            where f xs = uiGroupGui gOff (ver xs)

uiGroupGui :: Gui -> Gui -> Gui 
uiGroupGui a b =ver [sca 1.7 a, sca 8 b]

sourceColor2 :: Color -> Source a -> Source a
sourceColor2 col a = source $ do
    (g, x) <- a
    return (setColor2 col g, x)

-- | Colors the source widgets.
fxColor :: Color -> Source a -> Source a
fxColor = sourceColor2

-- combine effects

fxGroup :: ([Gui] -> Gui) -> [Source FxFun] -> Source FxFun
fxGroup guiGroup as = do
    (gs, fs) <- fmap unzip $ sequence as    
    return (guiGroup gs, foldl (\a b -> a >=> b) return fs)

-- | Scales the gui for signal processing widgets.
fxSca :: Double -> Source FxFun -> Source FxFun
fxSca d a = fxGroup (\xs -> sca d $ head xs) [a]

-- | Groups the signal processing widgets. 
-- The functions are composed the visuals are
-- grouped  horizontaly.
fxHor :: [Source FxFun] -> Source FxFun
fxHor = fxGroup hor

-- | Groups the signal processing widgets. 
-- The functions are composed the visuals are
-- grouped  verticaly.
fxVer :: [Source FxFun] -> Source FxFun
fxVer = fxGroup ver

-- | Applies a function to a signal processing function.
fxApp :: FxFun -> Source FxFun -> Source FxFun 
fxApp f = mapSource (>=> f)

-- | The distortion widget. The arguments are
--
-- > uiDistort isOn levelOfDistortion drive tone
uiDistort :: Bool -> Double -> Double -> Double -> Source FxFun
uiDistort isOn level drive tone = sourceColor2 C.red $ fxBox "Distortion" fxDistort2 isOn 
    [("level", level), ("drive", drive), ("tone", tone)]

-- | The chorus widget. The arguments are
--
-- > uiChorus isOn mix rate depth width 
uiChorus :: Bool -> Double -> Double -> Double -> Double -> Source FxFun
uiChorus isOn mix rate depth width = sourceColor2 C.coral $ fxBox "Chorus" stChorus2 isOn
    [("mix",mix), ("rate",rate), ("depth",depth), ("width",width)]

-- | The flanger widget. The arguments are
--
-- > uiFlanger isOn mix feedback rate depth delay
uiFlanger :: Bool -> Double -> Double -> Double -> Double -> Double -> Source FxFun
uiFlanger isOn mix fback rate depth delay = sourceColor2 C.indigo $ fxBox "Flanger" fxFlanger2 isOn
    [("mix", mix), ("fback", fback), ("rate",rate), ("depth",depth), ("delay",delay)]   

-- | The phaser widget. The arguments are
--
-- > uiPhaser isOn mix feedback rate depth frequency
uiPhaser :: Bool -> Double -> Double -> Double -> Double -> Double -> Source FxFun
uiPhaser isOn mix fback rate depth freq = sourceColor2 C.orange $ fxBox "Phaser" fxPhaser2 isOn
    [("mix", mix), ("fback", fback), ("rate",rate), ("depth",depth), ("freq", freq)]

-- | The delay widget. The arguments are
--
-- > uiDelay isOn mix feedback delayTime tone
uiDelay :: Bool -> Double -> Double -> Double -> Double -> Source FxFun
uiDelay isOn mix fback time tone = sourceColor2 C.dodgerblue $ fxBox "Delay" analogDelay2 isOn
    [("mix",mix), ("fback",fback), ("time",time), ("tone",tone)]

-- | The simplified delay widget. The arguments are
--
-- > uiEcho isOn maxDelayTime delayTime feedback
uiEcho :: Bool -> D -> Double -> Double -> Source FxFun
uiEcho isOn maxDelTime time fback = sourceColor2 C.deepskyblue $ fxBox "Echo" (fxEcho2 maxDelTime) isOn
    [("time", time), ("fback", fback)]

-- | The pair of low and high pass filters
--
-- > uiFilter isOn lowPassfrequency highPassFrequency gain
uiFilter :: Bool -> Double -> Double -> Double -> Source FxFun
uiFilter isOn lpf hpf gain = fxBox "Filter" fxFilter2 isOn
    [("lpf",lpf), ("hpf",hpf), ("gain",gain)]

-- | The reverb widget. The arguments are:
--
-- > uiReverb mix depth
uiReverb :: Bool -> Double -> Double -> Source FxFun
uiReverb isOn mix depth = sourceColor2 C.forestgreen $ fxBox "Reverb" (\mix depth asig -> mul (1 - mix) asig + mul mix (rever2 depth asig)) isOn
    [("mix", mix), ("depth", depth)]

-- | The gain widget. The arguments are
--
-- > uiGain isOn amountOfGain
uiGain :: Bool -> Double -> Source FxFun
uiGain isOn gain = sourceColor2 C.black $ fxBox "Gain" fxGain isOn [("gain", gain)]

-- | The filtered white noize widget. The arguments are
--
-- > uiWhite isOn centerFreqOfFilter amountOfNoize 
uiWhite :: Bool -> Double -> Double -> Source FxFun
uiWhite isOn freq depth = sourceColor2 C.dimgray $ fxBox "White" fxWhite2 isOn 
    [("freq", freq), ("depth", depth)]

-- | The filtered pink noize widget. The arguments are
--
-- > uiPink isOn centerFreqOfFilter amountOfNoize 
uiPink :: Bool -> Double -> Double -> Source FxFun
uiPink isOn freq depth = sourceColor2 C.deeppink $ fxBox "Pink" fxPink2 isOn
    [("freq", freq), ("depth", depth)]

-- | The constructor for signal processing functions with no arguments (controlls).
uiFx :: FxUI a => String -> a -> Bool -> Source FxFun
uiFx name f isOn = fxBox name f isOn [] 

-- | The reverb for room.
uiRoom :: Bool -> Source FxFun
uiRoom isOn = sourceColor2 C.limegreen $ uiFx "Room" smallRoom2 isOn

-- | The reverb for hall.
uiHall :: Bool -> Source FxFun
uiHall isOn = sourceColor2 C.mediumseagreen $ uiFx "Hall" largeHall2 isOn

-- | The reverb for magic cave.
uiCave :: Bool -> Source FxFun
uiCave isOn = sourceColor2 C.darkviolet $ uiFx "Cave" magicCave2 isOn

-- | The widget for selecting a midi instrument. 
uiMidi :: Bool -> [(String, Msg -> SE Sig2)] -> Source FxFun
uiMidi isOn as = sourceColor2 C.forestgreen $ undefined

-- | the widget for mixing in a signal to the signal.
uiSig :: String -> Bool -> Source Sig2 -> Source FxFun
uiSig name onOff widget = source $ do
    (gs, asig) <- widget
    (gOff0, off) <- toggleSig name onOff
    let gOff = setFontSize 25 gOff0     
        f x = return $ x + mul (portk off 0.05) asig  
    return (setBorder UpBoxBorder $ uiGroupGui gOff gs, f)

-- | A mixer widget represented as an effect.
-- The effect sums the signals with given wieghts.
uiMix :: Bool -> [(String, SE Sig2)] -> Source FxFun
uiMix onOff as = sourceColor2 C.blue $ uiSig "Mix" onOff (mixer as)

----------------------------------------------------------------------
-- Widgets

data AdsrBound = AdsrBound
    { attBound  :: Double
    , decBound  :: Double
    , relBound  :: Double }

data AdsrInit = AdsrInit
    { attInit   :: Double
    , decInit   :: Double
    , susInit   :: Double
    , relInit   :: Double }

expEps :: Fractional a => a
expEps = 0.00001

linAdsr :: String -> AdsrBound -> AdsrInit -> Source Sig
linAdsr = genAdsr $ \a d s r -> linsegr [0, a, 1, d, s] r 0

expAdsr :: String -> AdsrBound -> AdsrInit -> Source Sig
expAdsr = genAdsr $ \a d s r -> expsegr [double expEps, a, 1, d, s] r (double expEps)

genAdsr :: (D -> D -> D -> D -> Sig)
    -> String -> AdsrBound -> AdsrInit -> Source Sig
genAdsr mkAdsr name b inits = source $ do
    (gatt, att) <- knob "A" (linSpan expEps $ attBound b) (attInit inits)
    (gdec, dec) <- knob "D" (linSpan expEps $ decBound b) (decInit inits)
    (gsus, sus) <- knob "S" (linSpan expEps 1)       (susInit inits) 
    (grel, rel) <- knob "R" (linSpan expEps $ relBound b) (relInit inits)
    let val   = mkAdsr (ir att) (ir dec) (ir sus) (ir rel)
    gui <- setTitle name $ hor [gatt, gdec, gsus, grel]
    return (gui, val)

-- | A widget with four standard waveforms: pure tone, triangle, square and sawtooth.
-- The last parameter is a default waveform (it's set at init time).
classicWaves :: String -> Int -> Source (Sig -> Sig)
classicWaves name initVal = funnyRadio name 
    [ ("osc", osc)
    , ("tri", tri)
    , ("sqr", sqr)
    , ("saw", saw)]
    initVal

-- | Slider for master volume
masterVolume :: Source Sig
masterVolume = slider "master" uspan 0.5

-- | Knob for master volume
masterVolumeKnob :: Source Sig
masterVolumeKnob = knob "master" uspan 0.5

