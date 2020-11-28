module Csound.Catalog.Wave.Sharc(
    -- * Oscillators
    sharcOsc, sigSharcOsc, rndSharcOsc, rndSigSharcOsc,
    soloSharcOsc, orcSharcOsc, purePadSharcOsc, padSharcOsc,

    -- * Padsynth
    PadSharcSpec(..), padsynthSharcOsc, padsynthSharcOsc2,
    padsynthSharcOsc', padsynthSharcOsc2',

    -- * Padsynth and granular
    morphsynthSharcOsc, morphsynthSharcOsc', quadMorphsynthSharcOsc, quadMorphsynthSharcOsc',

    -- * Instriments
    SharcInstr(..),
    shViolin, shViolinPizzicato, shViolinMuted, shViolinMarteleBowing, shViolinsEnsemble, shViola, shViolaPizzicato, shViolaMuted,
    shViolaMarteleBowing, shTuba, shTromboneMuted, shTrombone, shPiccolo, shOboe, shFrenchHornMuted, shFrenchHorn, shFlute,
    shEnglishHorn, shClarinetEflat, shTrumpetMutedC, shTrumpetC, shContrabassClarinet, shContrabassoon, shCello, shCelloPizzicato,
    shCelloMuted, shCelloMarteleBowing, shContrabassPizzicato, shContrabassMuted, shContrabassMarteleBowing, shContrabass,
    shClarinet, shBassTrombone, shBassClarinet, shBassoon, shBassFlute, shTrumpetBach, shAltoTrombone, shAltoFlute,

    -- * Low-level getters
    getInstrTab, note2sig, note2tab
) where

import qualified Sharc.Types as Sh
import qualified Sharc.Data as Sh
import Csound.Base
import Sharc.Types

note2sig :: Sh.Note -> Sig
note2sig n = oscBy (harmonics2tab $ Sh.noteHarmonics n) (sig $ double $ Sh.pitchFund $ Sh.notePitch n)

note2tab :: Sh.Note -> Tab
note2tab n = (harmonics2tab $ Sh.noteHarmonics n)

deg x = 180 * x / pi

harmonics2tab harmonics = sines3 $ fmap (\h -> (fromIntegral $ Sh.harmonicId h, Sh.harmonicAmplitude h, deg $ Sh.harmonicPhase h)) harmonics

-- | Get instrument wave table by midi pitch number.
getInstrTab :: SharcInstr -> Int -> Tab
getInstrTab (SharcInstr instr) n = note2tab $ Sh.instrNotes instr !! idx
    where
        ns = Sh.instrNotes instr
        keys = fmap (Sh.pitchKeyNum . Sh.notePitch) ns
        keyMin = minimum keys
        keyMax = maximum keys
        idx = (min (max keyMin n) keyMax - keyMin)

---------------------------------------------------------------------------
-- oscilliators

-- | Sharc oscillator
sharcOsc :: SharcInstr -> D -> Sig
sharcOsc instr cpsTab = sigSharcOsc instr cpsTab (sig cpsTab)

-- | Sharc oscillator with continuous pitch.
-- The second argument picks upth table by frequency
-- and the third supplies the frequency.
sigSharcOsc :: SharcInstr -> D -> Sig -> Sig
sigSharcOsc = genSharcOsc' oscBy 

-- | Sharc oscillator with randomized phase.
rndSharcOsc :: SharcInstr -> D -> SE Sig
rndSharcOsc instr cpsTab = rndSigSharcOsc instr cpsTab (sig cpsTab)

-- | Sharc oscillator with continuous pitch and randomized phase.
rndSigSharcOsc :: SharcInstr -> D -> Sig -> SE Sig
rndSigSharcOsc = genSharcOsc' rndOscBy 

genSharcOsc' :: (Tab -> Sig -> a) -> SharcInstr -> D -> Sig -> a
genSharcOsc' wave (SharcInstr instr) cps cpsSig = wave t cpsSig
    where
        t = fromTabListD tabs (cps2pitch cps - int keyMin)        

        tabs = tabList $ fmap note2tab ns

        ns = Sh.instrNotes instr
        keys = fmap (Sh.pitchKeyNum . Sh.notePitch) ns
        keyMin = minimum keys        

cps2pitch :: Floating a => a -> a
cps2pitch x =  69 + 12 * logBase 2 (x / 440)

---------------------------------------------------------------------------
-- patches

uni = multiHz 4 (cent 40)

-- | Plays a solo instrument
soloSharcOsc :: SharcInstr -> D -> SE Sig
soloSharcOsc instr cps = mul (fades 0.001 0.05) $ rndSharcOsc instr cps

-- | Plays a orchestrated instrument (with pitch chorus)
orcSharcOsc :: SharcInstr -> D -> SE Sig
orcSharcOsc instr cps = mul (fades 0.01 0.42) $ uni (rndSigSharcOsc instr cps) (sig cps)

-- | Plays a solo instrument with pad-like envelope
purePadSharcOsc :: SharcInstr -> D -> SE Sig
purePadSharcOsc instr cps = mul (fades 0.65 0.75) $ rndSharcOsc instr cps

-- | Plays orchestrated instrument with pad-like envelope
padSharcOsc :: SharcInstr -> D -> SE Sig
padSharcOsc instr cps = mul (fades 0.65 0.75) $ uni (rndSigSharcOsc instr cps) (sig cps)

---------------------------------------------------------------------------
-- padsynth

data PadSharcSpec = PadSharcSpec {
        padSharcBandwidth :: Double,
        padSharcSize      :: Int
    }

instance Default PadSharcSpec where
    def = PadSharcSpec 15 8

padsynthSharcOsc :: SharcInstr -> D -> SE Sig
padsynthSharcOsc = padsynthSharcOsc' def

padsynthSharcOsc2 :: SharcInstr -> D -> SE Sig2
padsynthSharcOsc2 = padsynthSharcOsc2' def

padsynthSharcOsc2' :: PadSharcSpec -> SharcInstr -> D -> SE Sig2
padsynthSharcOsc2' spec instr freq = padsynthOscMultiCps2 (getSpecIntervals spec instr) freq

padsynthSharcOsc' :: PadSharcSpec -> SharcInstr -> D -> SE Sig
padsynthSharcOsc' spec instr freq = padsynthOscMultiCps (getSpecIntervals spec instr) freq

morphsynthSharcOsc :: MorphSpec -> SharcInstr -> D -> SE Sig2
morphsynthSharcOsc = morphsynthSharcOsc' def

morphsynthSharcOsc' :: PadSharcSpec -> MorphSpec -> SharcInstr -> D -> SE Sig2
morphsynthSharcOsc' spec morphSpec instr freq = morphsynthOscMultiCps morphSpec (getSpecIntervals spec instr) freq

quadMorphsynthSharcOsc :: MorphSpec -> [SharcInstr] -> (Sig, Sig) -> D -> SE Sig2
quadMorphsynthSharcOsc = quadMorphsynthSharcOsc' def

quadMorphsynthSharcOsc' :: PadSharcSpec -> MorphSpec -> [SharcInstr] -> (Sig, Sig) -> D -> SE Sig2
quadMorphsynthSharcOsc' spec morphSpec instr (x, y) freq = quadMorphsynthOscMultiCps morphSpec (fmap (getSpecIntervals spec) instr) (x, y) freq

getSpecIntervals spec (SharcInstr instr) = zip borderFreqs specs
    where 
        groups = splitTo (padSharcSize spec) (Sh.instrNotes instr)
        medians = fmap getMedian groups
        borders = fmap getBorder groups

        specs   = fmap (note2padsynth $ padSharcBandwidth spec) medians
        borderFreqs = fmap (Sh.pitchFund . Sh.notePitch) borders


splitTo :: Int -> [a] -> [[a]]
splitTo n as = go size as
    where 
        size = max 1 (length as `div` n)

        go :: Int -> [a] -> [[a]]
        go n bs
            | null ys   = [xs]
            | otherwise = xs : go n ys
            where
                (xs, ys) = splitAt n bs

getMedian :: [a] -> a
getMedian as
    | null as   = error "getMedian: Csound.Catalog.Wave.Sharc.hs empty list"
    | otherwise = as !! (length as `div` 2)

getBorder :: [a] -> a
getBorder as
    | null as   = error "getMedian: Csound.Catalog.Wave.Sharc.hs empty list"
    | otherwise = last as

note2padsynth :: Double -> Sh.Note -> PadsynthSpec
note2padsynth bandwidth note = (defPadsynthSpec bandwidth normAmps) { padsynthFundamental = Sh.pitchFund (Sh.notePitch note) }
    where 
        normAmps = fmap ( / maxAmp) amps
        amps = fmap Sh.harmonicAmplitude $ Sh.noteHarmonics note
        maxAmp = maximum amps


toStereoOsc :: (a -> SE Sig) -> (a -> SE Sig2)
toStereoOsc f x = do
    left  <- f x
    right <- f x
    return (left, right)

---------------------------------------------------------------------------
-- sharc instr

newtype SharcInstr = SharcInstr { unSharcInstr :: Sh.Instr }

shViolin :: SharcInstr
shViolin = SharcInstr Sh.violin

shViolinPizzicato :: SharcInstr
shViolinPizzicato = SharcInstr Sh.violinPizzicato

shViolinMuted :: SharcInstr
shViolinMuted = SharcInstr Sh.violinMuted

shViolinMarteleBowing :: SharcInstr
shViolinMarteleBowing = SharcInstr Sh.violinMarteleBowing

shViolinsEnsemble :: SharcInstr
shViolinsEnsemble = SharcInstr Sh.violinsEnsemble

shViola :: SharcInstr
shViola = SharcInstr Sh.viola

shViolaPizzicato :: SharcInstr
shViolaPizzicato = SharcInstr Sh.violaPizzicato

shViolaMuted :: SharcInstr
shViolaMuted = SharcInstr Sh.violaMuted

shViolaMarteleBowing :: SharcInstr
shViolaMarteleBowing = SharcInstr Sh.violaMarteleBowing

shTuba :: SharcInstr
shTuba = SharcInstr Sh.tuba

shTromboneMuted :: SharcInstr
shTromboneMuted = SharcInstr Sh.tromboneMuted

shTrombone :: SharcInstr
shTrombone = SharcInstr Sh.trombone

shPiccolo :: SharcInstr
shPiccolo = SharcInstr Sh.piccolo

shOboe :: SharcInstr
shOboe = SharcInstr Sh.oboe

shFrenchHornMuted :: SharcInstr
shFrenchHornMuted = SharcInstr Sh.frenchHornMuted

shFrenchHorn :: SharcInstr
shFrenchHorn = SharcInstr Sh.frenchHorn

shFlute :: SharcInstr
shFlute = SharcInstr Sh.flute

shEnglishHorn :: SharcInstr
shEnglishHorn = SharcInstr Sh.englishHorn

shClarinetEflat :: SharcInstr
shClarinetEflat = SharcInstr Sh.clarinetEflat

shTrumpetMutedC :: SharcInstr
shTrumpetMutedC = SharcInstr Sh.trumpetMutedC

shTrumpetC :: SharcInstr
shTrumpetC = SharcInstr Sh.trumpetC

shContrabassClarinet :: SharcInstr
shContrabassClarinet = SharcInstr Sh.contrabassClarinet

shContrabassoon :: SharcInstr
shContrabassoon = SharcInstr Sh.contrabassoon

shCello :: SharcInstr
shCello = SharcInstr Sh.cello

shCelloPizzicato :: SharcInstr
shCelloPizzicato = SharcInstr Sh.celloPizzicato

shCelloMuted :: SharcInstr
shCelloMuted = SharcInstr Sh.celloMuted

shCelloMarteleBowing :: SharcInstr
shCelloMarteleBowing = SharcInstr Sh.celloMarteleBowing

shContrabassPizzicato :: SharcInstr
shContrabassPizzicato = SharcInstr Sh.contrabassPizzicato

shContrabassMuted :: SharcInstr
shContrabassMuted = SharcInstr Sh.contrabassMuted

shContrabassMarteleBowing :: SharcInstr
shContrabassMarteleBowing = SharcInstr Sh.contrabassMarteleBowing

shContrabass :: SharcInstr
shContrabass = SharcInstr Sh.contrabass

shClarinet :: SharcInstr
shClarinet = SharcInstr Sh.clarinet

shBassTrombone :: SharcInstr
shBassTrombone = SharcInstr Sh.bassTrombone

shBassClarinet :: SharcInstr
shBassClarinet = SharcInstr Sh.bassClarinet

shBassoon :: SharcInstr
shBassoon = SharcInstr Sh.bassoon

shBassFlute :: SharcInstr
shBassFlute = SharcInstr Sh.bassFlute

shTrumpetBach :: SharcInstr
shTrumpetBach = SharcInstr Sh.trumpetBach

shAltoTrombone :: SharcInstr
shAltoTrombone = SharcInstr Sh.altoTrombone

shAltoFlute :: SharcInstr
shAltoFlute = SharcInstr Sh.altoFlute