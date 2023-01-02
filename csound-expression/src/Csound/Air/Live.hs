{-# Language ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- | UIs for live performances
module Csound.Air.Live (
    -- * Mixer
    mixer, hmixer, mixMono,

    -- * Effects
    fxBox, uiBox,
    fxColor, fxVer, fxHor, fxGrid, fxSca, fxMap, fxApply, atFx,
    fxHorMS, fxVerMS, fxGridMS,
    fromMonoFx,

    -- * Instrument choosers
    hinstrChooser, vinstrChooser,
    hmidiChooser, vmidiChooser,
--    hpatchChooser, vpatchChooser,

    -- ** Fx units
    uiDistort, uiChorus, uiFlanger, uiPhaser, uiDelay, uiEcho, uiFilter, uiReverb,
    uiGain, uiCompress, uiWhite, uiPink, uiFx, uiDry,
    uiSig, uiMix, uiMidi,
    -- uiPatch,

     -- * Static widgets
    AdsrBound(..), AdsrInit(..),
    linAdsr, expAdsr,
    classicWaves,
    masterVolume, masterVolumeKnob,

    -- * Live row
    LiveClip(..), ClipParam(..),
    liveRow, liveRows,
    ambiRow, ambiRowMp3
) where

import Control.Monad

import Data.Bool
import Data.Boolean
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Default
import qualified Data.Colour.Names as C
import qualified Data.Colour.SRGB as C

import Csound.Typed hiding (arg, mix)
import Csound.Typed.Gui hiding (widget, width)
import Csound.Control.Instr hiding (mix)
import Csound.Control.Gui hiding (widget, width)
import Csound.Typed.Opcode hiding (space, integ, gain, tone, delay, mute)
import Csound.Air.Wave
import Csound.Air.Fx
import Csound.Air.Patch
import Csound.Air.Misc
import Csound.Tab

import qualified Csound.Typed.Plugins as P

----------------------------------------------------------------------
-- mixer

-- | Widget that represents a mixer.
mixer :: (Sigs a) => [(Text, SE a)] -> Source a
mixer = genMixer (ver, hor)

-- | Widget that represents a mixer with horizontal grouping of elements.
hmixer :: (Sigs a) => [(Text, SE a)] -> Source a
hmixer = genMixer (hor, ver)

genMixer :: (Sigs a) => ([Gui] -> Gui, [Gui] -> Gui) -> [(Text, SE a)] -> Source a
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
    res <- fmap (mul masterMuteVol . mean) $ zipWithM (\v ain -> fmap (mul v) ain) muteVols sigs
    return (g, res)
    where
        (names, sigs) = unzip as
        appMute mute vol = (port (1 - mute) 0.05) * vol

-- | Transforms the mono signal to the stereo input
-- for the mixer widget.
mixMono :: Text -> Sig -> (Text, SE Sig2)
mixMono name asig = (name, return (asig, asig))

defSlider :: Text -> Source Sig
defSlider tag = slider tag (linSpan 0 1) 0.5

----------------------------------------------------------------------
-- effects

-- | Creates a widget that represents a stereo signal processing function.
-- The parameters of the widget are updated with sliders.
-- For example let's create a simple gain widget. It can be encoded like this:
--
-- > uiGain :: Double -> Source FxFun
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
-- fxBox :: forall a. (FxUI a, Num  (FxArg a), Tuple (FxArg a)) => Text -> a -> Bool -> [(Text, Double)] -> Source (Fx (FxArg a))
fxBox :: forall a. Sigs a => Text -> ([Sig] -> Fx a) -> Bool -> [(Text, Double)] -> Source (Fx a)
fxBox name fx onOff args = source $ do
    (gOff0, off) <- toggleSig name onOff
    let gOff = setFontSize 25 gOff0
    offRef <- newGlobalRef (0 :: Sig)
    writeRef offRef off
    let (names, initVals) = unzip args
    (gs, as)  <- fmap unzip $ mapM (\(nm, initVal) -> slider nm (linSpan 0 1) initVal) $ zip names initVals
    let f x = do
          ref <- newRef (0 :: a)
          goff <- readRef offRef
          writeRef ref x
          when1 (goff ==* 1) $ do
              x2 <- readRef ref
              writeRef ref =<< fx as x2
          res <- readRef ref
          return res
    let gui = setBorder UpBoxBorder $ go (length names) gOff gs
    return (gui, f)
    where
        go n gOff gs
            | n == 0 = gOff
            | n < 4  = f (gs ++ replicate (4 - n) space)
            | otherwise = f gs
            where f xs = uiGroupGui gOff (ver xs)

-- | Creates an FX-box from the given visual representation.
-- It inserts a big On/Off button atop of the GUI.
uiBox :: (Sigs a) => Text -> Source (Fx a) -> Bool -> Source (Fx a)
uiBox name fx' onOff =
  mapGuiSource (setBorder UpBoxBorder) $ vlift2' uiOnOffSize uiBoxSize go offs fx'
    where
        offs = mapGuiSource (setFontSize 25) $ toggleSig name onOff
        go off fx arg = fmap (mul off) $ fx arg

uiOnOffSize :: Double
uiOnOffSize = 1.7

uiBoxSize :: Double
uiBoxSize   = 8

uiGroupGui :: Gui -> Gui -> Gui
uiGroupGui a b =ver [sca uiOnOffSize a, sca uiBoxSize b]

sourceColor2 :: Color -> Source a -> Source a
sourceColor2 col a = source $ do
    (g, x) <- a
    return (setColor2 col g, x)

-- | Colors the source widgets.
fxColor :: Color -> Source a -> Source a
fxColor = sourceColor2

-- combine effects

fxGroupMS :: ([Gui] -> Gui) -> [Source Fx1] -> Maybe (Source (Sig -> SE Sig2)) -> [Source Fx2] -> Source (Sig -> SE Sig2)
fxGroupMS guiGroup as bridge bs = do
    (gsA, fA) <- getChain as
    (gsB, fB) <- getChain bs
    case bridge of
        Nothing -> return $ (guiGroup $ gsA ++ gsB, fA >=> fB . fromMono)
        Just widget -> do
            (gBridge, fBridge) <- widget
            return $ (guiGroup $ gsA ++ gBridge : gsB, fA >=> fBridge >=> fB)
    where
        getChain xs = do
            (gs, fs) <- fmap unzip $ sequence xs
            return (gs, foldl (\a b -> a >=> b) return fs)

fxGroup :: ([Gui] -> Gui) -> [Source (Fx a)] -> Source (Fx a)
fxGroup guiGroup as = do
    (gs, fs) <- fmap unzip $ sequence as
    return (guiGroup gs, foldl (\a b -> a >=> b) return fs)

-- | Scales the gui for signal processing widgets.
fxSca :: Double -> Source (Fx a) -> Source (Fx a)
fxSca d a = fxGroup (\xs -> sca d $ head xs) [a]

-- | Groups the signal processing widgets.
-- The functions are composed the visuals are
-- grouped  horizontally.
fxHor :: [Source (Fx a)] -> Source (Fx a)
fxHor = fxGroup hor

-- | Groups the signal processing widgets.
-- The functions are composed the visuals are
-- grouped  vertically.
fxVer :: [Source (Fx a)] -> Source (Fx a)
fxVer = fxGroup ver

-- | Creates a matrix of fx-boxes.
--
-- > fxGrid columnsSize fxs
--
-- first argument is a number of columns in each row.
fxGrid :: Int -> [Source (Fx a)] -> Source (Fx a)
fxGrid columnsSize fxs = fxGroup (grid columnsSize) fxs

-- | @fxHor@ for chain that starts with mono effects and proceeds with stereo effects.
-- The second argument can contain The transition widget (mono to stereo effect) or it can be empty.
--  If it's empty automatic conversion will be inserted.
fxHorMS :: [Source Fx1] -> Maybe (Source (Sig -> SE Sig2)) -> [Source Fx2] -> Source (Sig -> SE Sig2)
fxHorMS = fxGroupMS hor

-- | @fxVer@ for chain that starts with mono effects and proceeds with stereo effects.
-- The second argument can contain The transition widget (mono to stereo effect) or it can be empty.
--  If it's empty automatic conversion will be inserted.
fxVerMS :: [Source Fx1] -> Maybe (Source (Sig -> SE Sig2)) -> [Source Fx2] -> Source (Sig -> SE Sig2)
fxVerMS = fxGroupMS ver

-- | Creates a matrix of fx-boxes. Stacks a list of mono and stereo FXs.
--
-- > fxGrid columnsSize monoFxs maybeBridge stereoFxs
--
-- first argument is a number of columns in each row.
fxGridMS :: Int -> [Source Fx1] -> Maybe (Source (Sig -> SE Sig2)) -> [Source Fx2] -> Source (Sig -> SE Sig2)
fxGridMS columnSize = fxGroupMS (grid columnSize)

-- | Applies FX with UI to the input argument.
fxApply :: Source (a -> SE b) -> a -> Source b
fxApply fx a = joinSource $ lift1 (\f -> f a) fx

-- | Applies a function to a signal processing function.
fxMap :: Fx a -> Source (Fx a) -> Source (Fx a)
fxMap f = mapSource (>=> f)

-- | Applies FX to the Patch.
atFx :: Source (Fx a) -> Patch a -> Source (Patch a)
atFx f patch = lift1 (\fx -> addPostFx 1 fx patch) f

-- | The distortion widget. The arguments are
--
-- > uiDistort isOn levelOfDistortion drive tone
uiDistort :: Sigs a => Bool -> Double -> Double -> Double -> Source (Fx a)
uiDistort isOn level drive tone = mapSource bindSig $ sourceColor2 C.red $
  fxBox "Distortion" (\[level', drive', tone'] -> return . fxDistort level' drive' tone') isOn
    [("level", level), ("drive", drive), ("tone", tone)]


-- | The chorus widget. The arguments are
--
-- > uiChorus isOn mix rate depth width
uiChorus :: Bool -> Double -> Double -> Double -> Double -> Source Fx2
uiChorus isOn mix rate depth width = sourceColor2 C.coral $
  fxBox "Chorus" (\[mix', rate', depth', width'] -> return . stChorus2 mix' rate' depth' width') isOn
    [("mix",mix), ("rate",rate), ("depth",depth), ("width",width)]

uiDry :: (Sigs a) => Source (Fx a)
uiDry = fxBox "Thru" (\[] -> return) True []

-- | The flanger widget. The arguments are
--
-- > uiFlanger isOn  rate depth delay feedback
uiFlanger :: Sigs a => Bool -> Double -> Double -> Double -> Double -> Source (Fx a)
uiFlanger isOn rate depth delay fback = mapSource bindSig $ sourceColor2 C.indigo $
  fxBox "Flanger" (\[fback', rate', depth', delay'] -> return . fxFlanger fback' rate' depth' delay') isOn
    [("rate",rate), ("depth",depth), ("delay",delay), ("fback", fback)]


-- | The phaser widget. The arguments are
--
-- > uiPhaser isOn mix feedback rate depth frequency
uiPhaser :: Sigs a => Bool -> Double -> Double -> Double -> Double -> Source (Fx a)
uiPhaser isOn rate depth freq fback = mapSource bindSig $ sourceColor2 C.orange $
  fxBox "Phaser" (\[rate', depth', frequency', feedback'] -> return . fxPhaser rate' depth' frequency' feedback') isOn
    [("rate",rate), ("depth",depth), ("freq", freq), ("fback", fback)]

-- | The delay widget. The arguments are
--
-- > uiDelay isOn mix feedback delayTime tone
uiDelay :: Sigs a => Bool -> Double -> Double -> Double -> Double -> Source (Fx a)
uiDelay isOn mix fback time tone = mapSource bindSig $ sourceColor2 C.dodgerblue $
  fxBox "Delay" (\[mix', fback', time', tone'] -> return . analogDelay mix' fback' time' tone') isOn
    [("mix",mix), ("fback",fback), ("time",time), ("tone",tone)]


-- | The simplified delay widget. The arguments are
--
-- > uiEcho isOn maxDelayTime delayTime feedback
uiEcho :: Sigs a => Bool -> D -> Double -> Double -> Source (Fx a)
uiEcho isOn maxDelTime time fback = mapSource bindSig $ sourceColor2 C.deepskyblue $
  fxBox "Echo" (\[time', fback'] -> return . fxEcho maxDelTime time' fback') isOn
    [("time", time), ("fback", fback)]


-- | The pair of low and high pass filters
--
-- > uiFilter isOn lowPassfrequency highPassFrequency gain
uiFilter :: Sigs a => Bool -> Double -> Double -> Double -> Source (Fx a)
uiFilter isOn lpf hpf gain = mapSource bindSig $
  fxBox "Filter" (\[lpf', hpf', gain'] -> return . fxFilter lpf' hpf' gain') isOn
    [("lpf",lpf), ("hpf",hpf), ("gain",gain)]


-- | The reverb widget. The arguments are:
--
-- > uiReverb mix depth
uiReverb :: Bool -> Double -> Double -> Source Fx2
uiReverb isOn mix depth = sourceColor2 C.forestgreen $
  fxBox "Reverb" (\[mix', depth'] asig -> return $ cfd mix' asig (rever2 depth' asig)) isOn
      [("mix", mix), ("depth", depth)]

-- | The gain widget, it's set to on by default. The arguments are
--
-- > uiGain amountOfGain
uiGain :: Sigs a => Double -> Source (Fx a)
uiGain gain = mapSource bindSig $ sourceColor2 C.black $
  fxBox "Gain" (\[vol] -> return . fxGain vol) True [("gain", gain)]

-- | The filtered white noize widget. The arguments are
--
-- > uiWhite isOn centerFreqOfFilter amountOfNoize
uiWhite :: Sigs a => Bool -> Double -> Double -> Source (Fx a)
uiWhite isOn freq depth = mapSource bindSig $ sourceColor2 C.dimgray $
  fxBox "White" (\[freq', depth'] -> fxWhite freq' depth') isOn
    [("freq", freq), ("depth", depth)]

-- | The filtered pink noize widget. The arguments are
--
-- > uiPink isOn centerFreqOfFilter amountOfNoize
uiPink :: Sigs a => Bool -> Double -> Double -> Source (Fx a)
uiPink isOn freq depth = mapSource bindSig $ sourceColor2 C.deeppink $
  fxBox "Pink" (\[freq', depth'] -> fxPink freq' depth') isOn
    [("freq", freq), ("depth", depth)]

-- | The constructor for signal processing functions with no arguments (controlls).
uiFx :: Sigs a => Text -> Fx a -> Bool -> Source (Fx a)
uiFx name f isOn = fxBox name (\[] -> f) isOn []

-- | Midi chooser implemented as FX-box.
uiMidi :: (Sigs a) => [(Text, Msg -> SE a)] -> Int -> Source (Fx a)
uiMidi xs initVal = sourceColor2 C.forestgreen $ uiBox "Midi" fx True
    where fx = lift1 (\aout arg -> return $ aout + arg) $ vmidiChooser xs initVal

{-
-- | Patch chooser implemented as FX-box.
uiPatch :: [(Text, Patch2)] -> Int -> Source FxFun
uiPatch xs initVal = sourceColor2 C.forestgreen $ uiBox "Patch" fx True
    where fx = lift1 (\aout arg -> return $ aout + arg) $ vpatchChooser xs initVal
-}

-- | the widget for mixing in a signal to the signal.
uiSig :: (Sigs a) => Text -> Bool -> Source a -> Source (Fx a)
uiSig name onOff widget = source $ do
    (gs, asig) <- widget
    (gOff0, off) <- toggleSig name onOff
    let gOff = setFontSize 25 gOff0
        f x = return $ x + mul (portk off 0.05) asig
    return (setBorder UpBoxBorder $ uiGroupGui gOff gs, f)

-- | A mixer widget represented as an effect.
-- The effect sums the signals with given wieghts.
uiMix :: (Sigs a) => Bool -> [(Text, SE a)] -> Source (Fx a)
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

linAdsr :: Text -> AdsrBound -> AdsrInit -> Source Sig
linAdsr = genAdsr $ \a d s r -> linsegr [0, a, 1, d, s] r 0

expAdsr :: Text -> AdsrBound -> AdsrInit -> Source Sig
expAdsr = genAdsr $ \a d s r -> expsegr [double expEps, a, 1, d, s] r (double expEps)

genAdsr :: (D -> D -> D -> D -> Sig)
    -> Text -> AdsrBound -> AdsrInit -> Source Sig
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
classicWaves :: Text -> Int -> Source (Sig -> Sig)
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


----------------------------------------------------
-- instrument choosers

genMidiChooser :: Sigs a => (t1 -> t2 -> Source (Msg -> SE a)) -> t1 -> t2 -> Source a
genMidiChooser chooser xs initVal = joinSource $ lift1 midi $ chooser xs initVal

-- | Chooses a midi instrument among several alternatives. It uses the @hradio@ for GUI groupping.
hmidiChooser :: Sigs a => [(Text, Msg -> SE a)] -> Int -> Source a
hmidiChooser = genMidiChooser hinstrChooser

-- | Chooses a midi instrument among several alternatives. It uses the @vradio@ for GUI groupping.
vmidiChooser :: Sigs a => [(Text, Msg -> SE a)] -> Int -> Source a
vmidiChooser = genMidiChooser vinstrChooser

-- | Chooses an instrument among several alternatives. It uses the @hradio@ for GUI groupping.
hinstrChooser :: (Sigs b) => [(Text, a -> SE b)] -> Int -> Source (a -> SE b)
hinstrChooser = genInstrChooser hradioSig

-- | Chooses an instrument among several alternatives. It uses the @vradio@ for GUI groupping.
vinstrChooser :: (Sigs b) => [(Text, a -> SE b)] -> Int -> Source (a -> SE b)
vinstrChooser = genInstrChooser vradioSig

genInstrChooser :: (Sigs b) => ([Text] -> Int -> Source Sig) -> [(Text, a -> SE b)] -> Int -> Source (a -> SE b)
genInstrChooser widget xs initVal = lift1 (routeInstr instrs) $ widget names initVal
    where (names, instrs) = unzip xs
        -- go instrId arg = fmap sum $ mapM ( $ arg) $ zipWith (\n instr -> playWhen (sig (int n) ==* instrId) instr) [0 ..] instrs

routeInstr :: Sigs b => [a -> SE b] -> Sig -> (a -> SE b)
routeInstr instrs instrId arg = fmap sum $ mapM ( $ arg) $ zipWith (\n instr -> playWhen (sig (int n) ==* instrId) instr) [0 ..] instrs

{-
----------------------------------------------------
-- effect choosers

hpatchChooser :: (SigSpace a, Sigs a) => [(Text, Patch D a)] -> Int -> Source a
hpatchChooser = genPatchChooser hradioSig

vpatchChooser :: (SigSpace a, Sigs a) => [(Text, Patch D a)] -> Int -> Source a
vpatchChooser = genPatchChooser vradioSig

genPatchChooser :: (SigSpace a, Sigs a) => ([Text] -> Int -> Source Sig) -> [(Text, Patch D a)] -> Int -> Source a
genPatchChooser widget xs initVal = joinSource $ lift1 go $ widget names initVal
    where
        (names, patches) = unzip xs
        go instrId = routeInstr fxs instrId =<< midi (routeInstr instrs instrId . ampCps)

        instrs = fmap patchInstr patches
        fxs    = fmap getPatchFx patches

-}


-------------------------------


-- | Compressor
--
-- > uiCompress thresh loknee hiknee ratio att rel gain
uiCompress :: Sigs a => Double -> Double -> Double -> Double -> Double -> Double -> Double -> Source (Fx a)
uiCompress initThresh initLoknee initHiknee initRatio initAtt initRel initGain = mapSource bindSig $ paintTo orange $ fxBox "Compress" fx True
    [("thresh", initThresh), ("loknee", initLoknee), ("hiknee", initHiknee), ("ratio", initRatio), ("att", initAtt), ("rel", initRel),  ("gain", initGain)]
    where
        fx [thresh, loknee, hiknee, ratio, att, rel, gain] = return . fxCompress thresh (loknee, hiknee) ratio (att, rel) gain
        fx _ = undefined

        paintTo = fxColor . C.sRGB24read
        orange = "#FF851B"

fromMonoFx :: Sigs a => (Sig -> Sig) -> Fx a
fromMonoFx f = \asig2 -> bindSig (return . f) asig2

-----------------------------------------------
-- live rows

-- | Live row triggers audio clips in sync.
--
-- > liveRow clips bpm barSize clipIndex
--
-- * @clips@ - contains file path to audio clips
--
-- * @bpm@ - the BPM of the track
--
-- * @barLength@ - length of the bar in quater notes. So 4 means 4/4
--
-- * @clipIndex@ - identity of the clip to launch on the next bar.
liveRow :: [LiveClip] -> D -> D -> Sig -> Sig
liveRow clips iBpm iBeatDur kUserIndex = P.liveRow iTabSize iTabs iBpm iBeatDur kUserIndex iAuxParams
    where
        iTabSize = int $ length clips
        iTabs = tabList $ fmap (wavLeft . liveClipFile) clips
        iAuxParams = getAuxClipParams clips

-- | Stereo version of liveRow
liveRows :: [LiveClip] -> D -> D -> Sig -> Sig2
liveRows clips iBpm iBeatDur kUserIndex = P.liveRows iTabSize iLeftTabs iRightTabs iBpm iBeatDur kUserIndex iAuxParams
    where
        iTabSize = int $ length clips
        iLeftTabs  = tabList $ fmap (wavLeft  . liveClipFile) clips
        iRightTabs = tabList $ fmap (wavRight . liveClipFile) clips
        iAuxParams = getAuxClipParams clips

-- | Clip and it's parameters
data LiveClip = LiveClip
    { liveClipFile  :: FilePath
    -- ^ path to the file of audio clip
    , liveClipParam :: ClipParam
    -- ^ clip launch parameters
    }

data ClipParam = ClipParam
    { clipParamSize     :: !Int
    -- ^ Clip size in bars
    , clipParamDel      :: !Int
    -- ^ Clip offset from beginning in bars
    , clipParamTail     :: !Int
    -- ^ Clip skip time at the end of the clip
    , clipParamNext     :: !Int
    -- ^ Next clip to play after this one is finished. If it's -1 then play the same clip
    , clipParamRetrig   :: !Bool
    -- ^ Should we retrigger clip from the start or continue play where we left out.
    , clipParamVol      :: !Double
    -- ^ Volume scaling factor for the clip
    }

instance Default ClipParam where
    def = ClipParam
        { clipParamSize   = -1
        , clipParamDel    = 0
        , clipParamTail   = 0
        , clipParamNext   = -1
        , clipParamRetrig = False
        , clipParamVol    = 1
        }

toClipParam :: ClipParam -> [Double]
toClipParam x =
        [ fromIntegral $ clipParamSize x
        , fromIntegral $ clipParamDel x
        , fromIntegral $ clipParamTail x
        , fromIntegral $ clipParamNext x
        , bool 0 1 (clipParamRetrig x)
        , clipParamVol x]

getAuxClipParams :: [LiveClip] -> Tab
getAuxClipParams xs = doubles $ fillTabToPowerOfTwo $
    toClipParam . liveClipParam =<< xs

fillTabToPowerOfTwo :: [Double]  -> [Double]
fillTabToPowerOfTwo xs = xs ++ replicate (nextPow - n) 0
    where
        n = length xs
        nextPow
            | frac == (0 :: Double) = n
            | otherwise = 2 ^ (integ + 1 :: Int)
            where
                (integ, frac) = properFraction $ logBase 2 (fromIntegral n)


ambiRow :: [String] -> Sig -> Sig -> D -> SE Sig2
ambiRow files kSpeed kIndex iFadeTime = do
  arr <- newGlobalCtrlArr [int $ length files]
  zipWithM_ (\n f -> writeArr arr n $ text f) (fmap (sig . int) [0..]) files
  return $ P.ambiRow arr kSpeed kIndex iFadeTime

ambiRowMp3 :: [String] -> Sig -> Sig -> D -> SE Sig2
ambiRowMp3 files kSpeed kIndex iFadeTime = do
  arr <- newGlobalCtrlArr [int $ length files]
  zipWithM_ (\n f -> writeArr arr n $ text f) (fmap (sig . int) [0..]) files
  return $ P.ambiRowMp3 arr kSpeed kIndex iFadeTime

