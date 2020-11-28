-- | Patches
--
-- Collection of beautiful timbres. To try the instrument with midi device just type in the interpreter:
--
-- > > dac $ atMidi hammondOrgan
-- If you don't have the real device, you can try the virtual midi:
--
-- > > vdac $ atMidi vibraphone1
--
-- The function @atMidi@ invokes a @Patch@ with midi.
module Csound.Patch(

	-- * Electric piano
	Epiano1(..), epiano1, epiano1',
	MutedPiano(..), mutedPiano, mutedPiano',
	amPiano, fmPiano,
	epiano2, epianoHeavy, epianoBright,
	vibraphonePiano1, vibraphonePiano2,
	addHammer,

	-- * Organ
	cathedralOrgan, toneWheelOrgan,
	HammondOrgan(..), hammondOrgan, hammondOrgan',
	sawOrgan, triOrgan, sqrOrgan, pwOrgan, waveOrgan,

	hammondOrganm, hammondOrganm', sawOrganm, triOrganm, sqrOrganm, pwOrganm, waveOrganm,

	-- * Accordeon
	accordeon, accordeonBright1, accordeonBright2, brokenAccordeon,
	accordeon', Accordeon(..),

	-- * Choir
	choirA, choirO, choirU, choirE,
	Choir(..), choirA', choirO', choirU', choirE',

	windSings, noisyChoir, longNoisyChoir, noisyChoir', longNoisyChoir', NoisyChoir(..),
	noisyRise, noisySpiral, noisySpiral',

	-- * Pad
	pwPad, triPad, nightPad, overtonePad, caveOvertonePad,
	chorusel, pwEnsemble, fmDroneSlow, fmDroneMedium, fmDroneFast, vibrophonePad,
	RazorPad(..), razorPadSlow, razorPadFast, razorPadTremolo, razorPad, razorPad',
	dreamPad, underwaterPad, lightIsTooBrightPad, whaleSongPad, dreamPadBy,
	dreamPad', underwaterPad', lightIsTooBrightPad', whaleSongPad', dreamPad',

	-- ** Pad Monosynth
	pwPadm, triPadm, nightPadm, overtonePadm, caveOvertonePadm, choruselm,
	pwEnsemblem, fmDroneSlowm, fmDroneMediumm, fmDroneFastm,
	razorPadSlowm, razorPadFastm, razorPadTremolom, razorPadm, razorPadm',
	dreamPadm, dreamPadBym, underwaterPadm, lightIsTooBrightPadm, whaleSongPadm, dreamPadm', underwaterPadm', dreamPadBym',
	lightIsTooBrightPadm', whaleSongPadm',

	-- * Lead
	polySynth,
	phasingLead, RazorLead(..), razorLeadSlow, razorLeadFast, razorLeadTremolo,
	razorLead, razorLead',
	overtoneLead,

	-- ** Lead Monosynth
	polySynthm, dafunkLead,

	-- * Bass
	simpleBass, pwBass, deepBass, withDeepBass,
	fmBass1, fmBass2,

	-- * Bowed
	celloSynt,

	-- * Plucked
	guitar, harpsichord,

	-- * Strikeh

	smallDahina, dahina, largeDahina, magicDahina,
	smallBanyan,banyan, largeBanyan, magicBanyan,
	smallXylophone, xylophone, largeXylophone, magicXylophone,
	smallTibetanBowl180, tibetanBowl180, largeTibetanBowl180, magicTibetanBowl180,
	smallSpinelSphere, spinelSphere, largeSpinelSphere, magicSpinelSphere,
	smallPotLid, potLid, largePotLid, magicPotLid,
	smallRedCedarWoodPlate, redCedarWoodPlate, largeRedCedarWoodPlate, magicRedCedarWoodPlate,
	smallTubularBell, tubularBell, largeTubularBell, magicTubularBell,
	smallRedwoodPlate, redwoodPlate, largeRedwoodPlate, magicRedwoodPlate, smallDouglasFirWoodPlate,
	douglasFirWoodPlate, largeDouglasFirWoodPlate, magicDouglasFirWoodPlate, smallUniformWoodenBar,
	uniformWoodenBar, largeUniformWoodenBar, magicUniformWoodenBar, smallUniformAluminumBar,
	uniformAluminumBar, largeUniformAluminumBar, magicUniformAluminumBar,
	smallVibraphone1, vibraphone1, largeVibraphone1, magicVibraphone1,
	smallVibraphone2, vibraphone2, largeVibraphone2, magicVibraphone2,
	smallChalandiPlates, chalandiPlates, largeChalandiPlates, magicChalandiPlates,
	smallTibetanBowl152, tibetanBowl152, largeTibetanBowl152, magicTibetanBowl152,
	smallTibetanBowl140, tibetanBowl140, largeTibetanBowl140, magicTibetanBowl140,
	smallWineGlass, wineGlass, largeWineGlass, magicWineGlass,
	smallHandbell, handbell, largeHandbell, magicHandbell,
	smallAlbertClockBellBelfast, albertClockBellBelfast, largeAlbertClockBellBelfast, magicAlbertClockBellBelfast,
	smallWoodBlock, woodBlock, largeWoodBlock, magicWoodBlock,

	-- * Scrape
	scrapeDahina, scrapeBanyan, scrapeXylophone, scrapeTibetanBowl180, scrapeSpinelSphere, scrapePotLid, scrapeRedCedarWoodPlate,
	scrapeTubularBell, scrapeRedwoodPlate, scrapeDouglasFirWoodPlate, scrapeUniformWoodenBar, scrapeUniformAluminumBar,
	scrapeVibraphone1, scrapeVibraphone2, scrapeChalandiPlates, scrapeTibetanBowl152, scrapeTibetanBowl140, scrapeWineGlass,
	scrapeSmallHandbell, scrapeAlbertClockBellBelfast, scrapeWoodBlock,

	scrapeFastDahina, scrapeFastBanyan, scrapeFastXylophone, scrapeFastTibetanBowl180, scrapeFastSpinelSphere, scrapeFastPotLid,
	scrapeFastRedCedarWoodPlate, scrapeFastTubularBell, scrapeFastRedwoodPlate, scrapeFastDouglasFirWoodPlate, scrapeFastUniformWoodenBar,
	scrapeFastUniformAluminumBar, scrapeFastVibraphone1, scrapeFastVibraphone2, scrapeFastChalandiPlates, scrapeFastTibetanBowl152,
	scrapeFastTibetanBowl140, scrapeFastWineGlass, scrapeFastSmallHandbell, scrapeFastAlbertClockBellBelfast, scrapeFastWoodBlock,

	scrapePadDahina, scrapePadBanyan, scrapePadXylophone, scrapePadTibetanBowl180, scrapePadSpinelSphere, scrapePadPotLid,
	scrapePadRedCedarWoodPlate, scrapePadTubularBell, scrapePadRedwoodPlate, scrapePadDouglasFirWoodPlate, scrapePadUniformWoodenBar,
	scrapePadUniformAluminumBar, scrapePadVibraphone1, scrapePadVibraphone2, scrapePadChalandiPlates, scrapePadTibetanBowl152,
	scrapePadTibetanBowl140, scrapePadWineGlass, scrapePadSmallHandbell, scrapePadAlbertClockBellBelfast, scrapePadWoodBlock,


	-- ** Scrape monosynth
	-- | Unfortunately they don't work with @atMonoMidi@. Though @atNote@ works fine.
	scrapeDahinam, scrapeBanyanm, scrapeXylophonem, scrapeTibetanBowl180m, scrapeSpinelSpherem, scrapePotLidm, scrapeRedCedarWoodPlatem,
	scrapeTubularBellm, scrapeRedwoodPlatem, scrapeDouglasFirWoodPlatem, scrapeUniformWoodenBarm, scrapeUniformAluminumBarm,
	scrapeVibraphone1m, scrapeVibraphone2m, scrapeChalandiPlatesm, scrapeTibetanBowl152m, scrapeTibetanBowl140m, scrapeWineGlassm,
	scrapeSmallHandbellm, scrapeAlbertClockBellBelfastm, scrapeWoodBlockm,

	scrapePadDahinam, scrapePadBanyanm, scrapePadXylophonem, scrapePadTibetanBowl180m, scrapePadSpinelSpherem, scrapePadPotLidm,
	scrapePadRedCedarWoodPlatem, scrapePadTubularBellm, scrapePadRedwoodPlatem, scrapePadDouglasFirWoodPlatem, scrapePadUniformWoodenBarm,
	scrapePadUniformAluminumBarm, scrapePadVibraphone1m, scrapePadVibraphone2m, scrapePadChalandiPlatesm, scrapePadTibetanBowl152m,
	scrapePadTibetanBowl140m, scrapePadWineGlassm, scrapePadSmallHandbellm, scrapePadAlbertClockBellBelfastm, scrapePadWoodBlockm,

	-- * Woodwind

	Wind(..), woodWind',

	fluteSpec, shortFluteSpec,
	flute, shortFlute, fluteVibrato, mutedFlute, brightFlute,

	bassClarinetSpec, shortBassClarinetSpec,
	bassClarinet, shortBassClarinet, bassClarinetVibrato, mutedBassClarinet, brightBassClarinet,

	frenchHornSpec, shortFrenchHornSpec,
	frenchHorn, shortFrenchHorn, frenchHornVibrato, mutedFrenchHorn, brightFrenchHorn,

	shengSpec, shortShengSpec,
	sheng, shortSheng, shengVibrato, mutedSheng, brightSheng,

	hulusiSpec, shortHulusiSpec,
	hulusi, shortHulusi, hulusiVibrato, mutedHulusi, brightHulusi,

	diziSpec, shortDiziSpec,
	dizi, shortDizi, diziVibrato, mutedDizi, brightDizi,

	-- * SHARC instruments
	SharcInstr,
	soloSharc, orcSharc, padSharc, purePadSharc,
	dreamSharc, lightIsTooBrightSharc, whaleSongSharc,
	sharcOrgan,

	-- ** Padsynth instruments
	PadSharcSpec(..),

	psOrganSharc, psOrganSharc', psLargeOrganSharc, psLargeOrganSharc', psPianoSharc, psPianoSharc',
	xpsPianoSharc, xpsPianoSharc',
	psPadSharc, psPadSharc', psSoftPadSharc, psSoftPadSharc',
	psMagicPadSharc, psMagicPadSharc', psMagicSoftPadSharc, psMagicSoftPadSharc',
	psLargePianoSharc, psLargePianoSharc',
	xpsLargePianoSharc,
	xpsLargePianoSharc',

	-- *** Deep pads
	psDeepPadSharc, psDeepPadSharc', psDeepSoftPadSharc, psDeepSoftPadSharc',
	psDeepMagicPadSharc, psDeepMagicPadSharc', psDeepMagicSoftPadSharc, psDeepMagicSoftPadSharc',

	--- *** Crossfades
	psPadSharcCfd, psPadSharcCfd', psPadSharcCfd4, psPadSharcCfd4', psDeepPadSharcCfd, psDeepPadSharcCfd',
	psDeepPadSharcCfd4, psDeepPadSharcCfd4', psSoftPadSharcCfd, psSoftPadSharcCfd', psSoftPadSharcCfd4, psSoftPadSharcCfd4',
	psDeepSoftPadSharcCfd, psDeepSoftPadSharcCfd', psDeepSoftPadSharcCfd4, psDeepSoftPadSharcCfd4',

	-- *** High resolution Padsynth instruments
	psOrganSharcHifi,
	psLargeOrganSharcHifi,
	psPianoSharcHifi,
	xpsPianoSharcHifi,
	psPadSharcHifi,
	psSoftPadSharcHifi,
	psMagicPadSharcHifi,
	psMagicSoftPadSharcHifi,
	psLargePianoSharcHifi,
	xpsLargePianoSharcHifi,

	-- *** Vedic pads
	-- | Deep spiritual pads.
	vedicPad, vedicPadCfd, vedicPadCfd4, vibhu, rishi, agni, prakriti, rajas, avatara, bhumi,

	--- *** High resolution vedic pads
	-- | Deep spiritual pads.
	vedicPadHifi, vibhuHifi, rishiHifi, agniHifi, prakritiHifi, rajasHifi, avataraHifi, bhumiHifi,

	--- *** Low resolution vedic pads
	-- | Deep spiritual pads.
	vedicPadLofi, vibhuLofi, rishiLofi, agniLofi, prakritiLofi, rajasLofi, avataraLofi, bhumiLofi,

	--- *** Crossfade vedic pads
	-- | Crossfade between deep spiritual pads. All pads take in padsynthBandwidth and crossfade level as parameters.
	vibhuRishi, vibhuAgni, vibhuPrakriti, vibhuRajas, vibhuAvatara, vibhuBhumi, rishiAgni, rishiPrakriti,
	rishiRajas, rishiAvatara, rishiBhumi, agniPrakriti, agniRajas, agniAvatara, agniBhumi, prakritiRajas,
	prakritiAvatara, prakritiBhumi, rajasAvatara, rajasBhumi, avataraBhumi,

	-- ** concrete instruments
	shViolin, shViolinPizzicato, shViolinMuted, shViolinMarteleBowing, shViolinsEnsemble, shViola, shViolaPizzicato, shViolaMuted,
    shViolaMarteleBowing, shTuba, shTromboneMuted, shTrombone, shPiccolo, shOboe, shFrenchHornMuted, shFrenchHorn, shFlute,
    shEnglishHorn, shClarinetEflat, shTrumpetMutedC, shTrumpetC, shContrabassClarinet, shContrabassoon, shCello, shCelloPizzicato,
    shCelloMuted, shCelloMarteleBowing, shContrabassPizzicato, shContrabassMuted, shContrabassMarteleBowing, shContrabass,
    shClarinet, shBassTrombone, shBassClarinet, shBassoon, shBassFlute, shTrumpetBach, shAltoTrombone, shAltoFlute,

	-- * X-rays
	pulseWidth, xanadu, alienIsAngry, noiz, blue, black, simpleMarimba, impulseMarimba1, impulseMarimba2, okComputer, noiseBell,

	-- * Robotic vowels
	robotVowels, robotLoopVowels, robotVowel,

	 -- ** Vowels
    maleA, maleE, maleIY, maleO, maleOO, maleU, maleER, maleUH,
    femaleA, femaleE, femaleIY, femaleO, femaleOO,

	-- * Nature
	windWall, mildWind, wind, snowCrackle,

	-- * Misc
	-- limRel,
	singleFx, singleFx'
) where

import Control.Monad

import Csound.Base

import qualified Csound.Catalog.Wave as C
import qualified Csound.Catalog.Reson as C

import Csound.Catalog.Wave(maleA, maleE, maleIY, maleO, maleOO, maleU, maleER, maleUH,
    femaleA, femaleE, femaleIY, femaleO, femaleOO)

import Csound.Catalog.Wave(Accordeon(..),
	ReleaseTime,
	SharcInstr,
	PadSharcSpec(..),
	shViolin, shViolinPizzicato, shViolinMuted, shViolinMarteleBowing, shViolinsEnsemble, shViola, shViolaPizzicato, shViolaMuted,
    shViolaMarteleBowing, shTuba, shTromboneMuted, shTrombone, shPiccolo, shOboe, shFrenchHornMuted, shFrenchHorn, shFlute,
    shEnglishHorn, shClarinetEflat, shTrumpetMutedC, shTrumpetC, shContrabassClarinet, shContrabassoon, shCello, shCelloPizzicato,
    shCelloMuted, shCelloMarteleBowing, shContrabassPizzicato, shContrabassMuted, shContrabassMarteleBowing, shContrabass,
    shClarinet, shBassTrombone, shBassClarinet, shBassoon, shBassFlute, shTrumpetBach, shAltoTrombone, shAltoFlute)

import Data.Char

monoArgToNote :: MonoArg -> (Sig, Sig)
monoArgToNote arg = (monoAmp arg * monoGate arg, monoCps arg)

monoSig1 :: SigSpace a => (Sig -> a) -> (MonoArg -> a)
monoSig1 f arg = mul env $ f cps
	where
		env = amp * monoAdsr arg 0.35 0.5 1 0.5
		amp = port (monoAmp arg) 0.01
		cps = portk (monoCps arg) (delay1 gate * 0.01)
		gate = monoGate arg

onSig1 :: SigSpace a => (Sig -> a) -> Sig2 -> a
onSig1 f (amp, cps) = mul amp $ f cps

fx1 :: Sig -> (a -> a) -> Patch a -> Patch a
fx1 dw f = FxChain [fxSpec dw (return . f)]

fx1' :: Sig -> (a -> SE a) -> Patch a -> Patch a
fx1' dw f = FxChain [fxSpec dw f]

-- | Creates a simple FX-xhain, that contains a single pure effect.
-- The first argument is the dry/wet-value.
singleFx :: Sig -> (a -> a) -> Patch a -> Patch a
singleFx = fx1

singleFxFilter :: Sig -> (ResonFilter -> a -> a) -> Patch a -> Patch a
singleFxFilter dw f = FxChain [fxSpecFilter dw (\filter x -> return $ f filter x)]

-- | Creates a simple FX-xhain, that contains a single effect.
-- The first argument is the dry/wet-value.
singleFx' :: Sig -> (a -> SE a) -> Patch a -> Patch a
singleFx' = fx1'

-- | Limits the release section of the note.
limRel :: SigSpace a => D -> Patch a -> Patch a
limRel rel p =  mapPatchInstr (\instr -> fmap (mul (fadeOut rel)) . instr) p

----------------------------------------------
-- electric pianos

data Epiano1 = Epiano1
	{ epiano1Rel :: D }

instance Default Epiano1 where
	def = Epiano1 5

epiano1 = epiano1' def

epiano1' (Epiano1 rel) = withLargeHall $ polySynt $ \a -> mul 0.4 $ C.simpleFading rel a

data MutedPiano = MutedPiano
	{ mutedPianoMute :: Sig
	, mutedPianoRel  :: D }

instance Default MutedPiano where
	def = MutedPiano 0.5 7

mutedPiano = mutedPiano' def

mutedPiano' (MutedPiano mute rel) = fx1 0.25 (largeHall2 . at (mlp3 (250 + 7000 * mute) 0.2)) $
	polySynt $ \a -> mul 0.7 $ C.simpleSust rel a

amPiano = fx1 0.25 id $ polySyntFilter $ \filter -> mul 1.4 . onCps (C.amPianoBy filter)

fmPiano = withSmallHall $ polySynt $ at fromMono . mul 0.75 . onCps (C.fmFlavio 6 3)

epianoReleaseTime :: ReleaseTime
epianoReleaseTime = 0.25

epiano2 = addHammer 0.15 $ fx1 0.25 smallHall2 $
	polySyntFilter $ \filter -> mul 1.125 . at fromMono . (onCps $ C.epianoBy filter epianoReleaseTime [C.EpianoOsc 4 5 1 1, C.EpianoOsc 8 10 2.01 1])

epianoHeavy = addHammer 0.15 $ fx1 0.2 smallHall2 $
	polySyntFilter $ \filter -> mul 1.125 . at fromMono . (onCps $ C.epianoBy filter epianoReleaseTime [C.EpianoOsc 4 5 1 1, C.EpianoOsc 8 10 2.01 1, C.EpianoOsc 8 15 0.5 0.5])

epianoBright = addHammer 0.15 $ fx1 0.2 smallHall2 $
	polySyntFilter $ \filter -> mul 1.12 . at fromMono . (onCps $ C.epianoBy filter epianoReleaseTime [C.EpianoOsc 4 5 1 1, C.EpianoOsc 8 10 3.01 1, C.EpianoOsc 8 15 5 0.5, C.EpianoOsc 8 4 7 0.3])

vibraphonePiano1 = vibraphoneToPiano smallVibraphone1
vibraphonePiano2 = vibraphoneToPiano smallVibraphone2

vibraphoneToPiano patch = addHammer 0.15 $ mapPatchInstr (\instr -> mul (1.5 * fadeOut 0.25) . at (mlp 6500 0.1) . instr) patch

-- | Adds a hammer strike sound. The first argument is the amount of hammer sound.
addHammer :: Sig -> Patch2 -> Patch2
addHammer amt = mixInstr amt impulseMarimba2

----------------------------------------------
-- organs

cathedralOrgan = withLargeHall $ polySynt $ at fromMono . mul 0.7 . onCps C.cathedralOrgan

-- [0, 30]
data HammondOrgan = HammondOrgan
	{ hammondOrganDetune :: Sig }

instance Default HammondOrgan where
	def = HammondOrgan 12

hammondOrgan = hammondOrgan' def

hammondOrganm = hammondOrganm' def

hammondOrgan' (HammondOrgan detune) = fx1 0.15 smallRoom2 $ polySynt $ mul 0.4 . at fromMono . onCps (C.hammondOrgan detune)

hammondOrganm' (HammondOrgan detune) = fx1 0.15 smallRoom2 $ monoSynt $ mul 0.4 . at fromMono . monoSig1 (C.hammondOrgan detune)

toneWheelOrgan = withSmallHall $ polySynt $ at fromMono  . mul (0.6 * fadeOut 0.05) . onCps C.toneWheel

sawOrgan  = mul 0.45 $ waveOrgan rndSaw
triOrgan  = mul 0.5  $ waveOrgan rndTri
sqrOrgan  = mul 0.45 $ waveOrgan rndSqr
pwOrgan k = mul 0.45 $ waveOrgan (rndPw k)

sawOrganm  = mul 0.45 $ waveOrganm rndSaw
triOrganm  = mul 0.5  $ waveOrganm rndTri
sqrOrganm  = mul 0.45 $ waveOrganm rndSqr
pwOrganm k = mul 0.45 $ waveOrganm (rndPw k)

organFx :: Patch2 -> Patch2
organFx = withSmallHall . singleFxFilter 1 (\filter -> at $ filter 3500 0.1)

waveOrgan :: (Sig -> SE Sig) -> Patch2
waveOrgan wave = organFx $ polySynt $ onCps $ at fromMono . mul (fades 0.01 0.01) . wave

waveOrganm :: (Sig -> SE Sig) -> Patch2
waveOrganm wave = organFx $ monoSynt $ monoSig1 $ at fromMono . mul (fades 0.01 0.01) . wave

waveOrganWithKey :: (D -> Sig -> SE Sig) -> Patch2
waveOrganWithKey wave = organFx $ polySynt $ onCps $ \cps -> (at fromMono . mul (fades 0.01 0.01) . wave cps) (sig cps)

----------------------------------------------
-- accordeons

accordeon = accordeon' def

accordeonBright1 = accordeon' (C.Accordeon 1 5 3 7)
accordeonBright2 = accordeon' (C.Accordeon 1 6 3 13)

accordeonHeavy = accordeon' (C.Accordeon 1 0.501 2 1.005)
brokenAccordeon = accordeon' (C.Accordeon 1 1.07 2.02 0.5)

accordeon' spec = fx1' 0.25 C.accordeonFx $ polySynt $ mul 0.63 . onCps (C.accordeon spec)

----------------------------------------------
-- choir

data Choir = Choir { choirVibr :: Sig }

instance Default Choir where
	def  = Choir 7

tenor'   filt (Choir vib) = withSmallHall $ polySynt $ at fromMono . mul 0.15 . onCps (C.tenorOsc filt vib)
soprano' filt (Choir vib) = withSmallHall $ polySynt $ at fromMono . mul 0.15 . onCps (C.sopranoOsc filt vib)

choir' filt vib = withSmallHall $ SplitPatch (dryPatch $ tenor' filt vib) 220 (dryPatch $ soprano' filt vib)

choirA = choirA' def
choirO = choirO' def
choirE = choirE' def
choirU = choirU' def

choirA' = choir' singA
choirO' = choir' singO
choirE' = choir' singE
choirU' = choir' singU

data NoisyChoir = NoisyChoir
	{ noisyChoirFilterNum :: Int
	, noisyChoirBw        :: Sig
	}

instance Default NoisyChoir where
	def = NoisyChoir 2 25

windSings = longNoisyChoir' (NoisyChoir 1 15)

longNoisyChoir = longNoisyChoir' def
noisyChoir = noisyChoir' def


dryNoisyChoir (NoisyChoir n bw) = polySynt $ at fromMono . mul 0.45 . onCps (C.noisyChoir n bw)

longNoisyChoir' ch = fx1 0.15 magicCave2 $ dryNoisyChoir ch

noisyChoir' ch = fx1 0.15 largeHall2 $ dryNoisyChoir ch

-- modes (wth delay or not delay)
--
--  dac $ mixAt 0.15 largeHall2 $ mixAt 0.2 (echo 0.25 0.45) $ at fromMono $ midi $ onMsg $ onCps (mul (fadeOut 2) . C.tibetanBowl152    )

----------------------------------------------
-- pads

pwPad  = withSmallHall $ polySyntFilter $ \filter -> mul 0.6 . at fromMono . onCps (C.pwPadBy filter)
pwPadm = withSmallHall $ monoSyntFilter $ \filter -> mul 0.6 . at fromMono . monoSig1 (C.pwPadBy filter)

triPad  = fx1' 0.25 C.triPadFx $ polySyntFilter $ \filter -> fmap fromMono . mul 0.7 . onCps (C.triPadBy filter)
triPadm = fx1' 0.25 C.triPadFx $ monoSyntFilter $ \filter -> fmap fromMono . mul 0.7 . monoSig1 (C.triPadBy filter)

nightPad  = withLargeHall $ polySynt $ mul 0.48 . at fromMono . onCps (mul (fadeOut 1) . C.nightPad 0.5)
nightPadm = withLargeHall $ monoSynt $ mul 0.48 . return . fromMono . monoSig1 ((fadeOut 1 * ) . C.nightPad 0.5)

overtoneFx     p = fx1 0.35 smallHall2 $ singleFxFilter 0.25 (\filter -> at (filter 1500 0.1)) p
caveOvertoneFx p = fx1 0.2  magicCave2 $ singleFxFilter 0.25 (\filter -> at (filter 1500 0.1)) $ mul 0.8 p

overtonePad  = overtoneFx $ polySynt $ mul 0.65 . at fromMono . onCps (\cps -> mul (fades 0.25 1.2) (C.tibetan 11 0.012 cps) + mul (fades 0.25 1) (C.tibetan 13 0.015 (cps * 0.5)))
overtonePadm = overtoneFx $ monoSynt $  mul 0.65 . return . fromMono . monoSig1 (\cps -> mul (fades 0.25 1.2) (C.tibetan 11 0.012 cps) + mul (fades 0.25 1) (C.tibetan 13 0.015 (cps * 0.5)))

caveOvertonePad =  caveOvertoneFx $ dryPatch overtonePad
caveOvertonePadm = caveOvertoneFx $ dryPatch overtonePadm

chorusel  = fx1 0.35 smallHall2 $ polySyntFilter $ \filter note -> (mul 0.9 . at (filter (3500 + 2000 * uosc 0.1) 0.1) . onCps (mul (fades 0.65 1) . C.chorusel 13 0.5 10)) note
choruselm = fx1 0.35 smallHall2 $ monoSyntFilter $ \filter note -> (mul 0.9 . return . at (filter (3500 + 2000 * uosc 0.1) 0.1) . monoSig1 (mul (fades 0.65 1) . C.chorusel 13 0.5 10)) note

pwEnsemble = withSmallHall $ polySyntFilter $ \filter -> at fromMono . mul 0.55 . onCps (C.pwEnsembleBy filter)
pwEnsemblem = withSmallHall $ monoSyntFilter $ \filter -> at fromMono . mul 0.55 . monoSig1 (C.pwEnsembleBy filter)

fmDroneSlow = fx1 0.35 largeHall2 $ polySynt $ at fromMono . mul 0.5 . onCps (C.fmDrone 3 (10, 5))
fmDroneSlowm = fx1 0.35 largeHall2 $ monoSynt $ return . at fromMono . mul 0.5 . monoSig1 (C.fmDrone 3 (10, 5))

fmDroneMedium = fx1 0.35 largeHall2 $ polySynt $ at fromMono . mul 0.5 . onCps (C.fmDrone 3 (5, 3))
fmDroneMediumm = fx1 0.35 largeHall2 $ monoSynt $ return . at fromMono . mul 0.5 . monoSig1 (C.fmDrone 3 (5, 3))

fmDroneFast = fx1 0.25 smallHall2 $ polySynt $ at fromMono . mul 0.5 . onCps (C.fmDrone 3 (0.5, 1))
fmDroneFastm = fx1 0.25 smallHall2 $ monoSynt $ return . at fromMono . mul 0.5 . monoSig1 (C.fmDrone 3 (0.5, 1))

vibrophonePad = addPreFx 1 (return . (at $ mlp 2500 0.1)) $ mapPatchInstr (\instr -> mul (1.5 * fades 0.5 0.25) . instr) largeVibraphone1

data RazorPad = RazorPad { razorPadSpeed :: Sig }

instance Default RazorPad where
	def = RazorPad 0.5

razorPadSlow = razorPad' (def { razorPadSpeed = 0.1 })
razorPadFast = razorPad' (def { razorPadSpeed = 1.7 })
razorPadTremolo = razorPad' (def { razorPadSpeed = 6.7 })

razorPadSlowm = razorPadm' (def { razorPadSpeed = 0.1 })
razorPadFastm = razorPadm' (def { razorPadSpeed = 1.7 })
razorPadTremolom = razorPadm' (def { razorPadSpeed = 6.7 })

razorPad = razorPad' def

razorPadm = razorPadm' def

razorPad'  (RazorPad speed) = fx1 0.35 largeHall2 $ polySyntFilter $ \filter -> at fromMono . mul 0.6 . onCps (uncurry $ C.razorPad filter speed)
razorPadm' (RazorPad speed) = fx1 0.35 largeHall2 $ monoSyntFilter $ \filter arg -> (at fromMono . mul 0.6 . (uncurry $ C.razorPad filter speed)) (monoArgToNote arg)

dreamPadFx = FxChain [fxSpec 0.35 (return . largeHall2), fxSpec 0.25 (return . (at $ echo 0.25 0.65)), fxSpec 0.25 (at $ chorus 0.07 1.25 1)]

dreamPad = dreamPad' 0.35
underwaterPad = underwaterPad' 0.35
lightIsTooBrightPad = lightIsTooBrightPad' 0.55
whaleSongPad = whaleSongPad' 0.35

dreamPadm = dreamPadm' 0.35
underwaterPadm = underwaterPadm' 0.35
lightIsTooBrightPadm = lightIsTooBrightPadm' 0.55
whaleSongPadm = whaleSongPadm' 0.35

dreamPadBym = dreamPadBym' 0.35

-- | The first argument is brightness (0 to 1)
dreamPad' :: Sig -> Patch2
dreamPad' bright = dreamPadFx $ polySyntFilter $ \filter note -> (fmap fromMono . onCps (C.dreamPad filter bright)) note

-- | The first argument is brightness. The second argument is a wave shape function.
dreamPadBy :: Sig -> Wave -> Patch2
dreamPadBy brightness wave = dreamPadFx $ polySyntFilter $ \filter note ->  (fmap fromMono . onCps (C.dreamPadBy filter wave brightness)) note

genDreamPadNote :: (ResonFilter -> Wave -> Sig -> Sig -> SE Sig) -> (D -> Wave) -> Sig -> ResonFilter -> Instr D Sig2
genDreamPadNote f wave brightness filter = fmap fromMono . onCps (\cps -> f filter (wave cps) brightness (sig cps))

-- genDreamPadNoteWithKey :: (ResonFilter -> (D -> Wave) -> Sig -> Sig -> SE Sig) -> Wave -> Sig -> ResonFilter -> Instr D Sig2
-- genDreamPadNoteWithKey f wave brightness filter = fmap fromMono . onCps (\cps -> f filter (wave cps) brightness (sig cps))

dreamPadWithKey :: Sig -> (D -> Sig -> SE Sig) -> Patch2
dreamPadWithKey brightness wave = dreamPadFx $ polySyntFilter $ genDreamPadNote C.dreamPadBy wave brightness

-- | The first argument is brightness (0 to 1)
dreamPadm' :: Sig -> Patch2
dreamPadm' bright = dreamPadFx $ monoSyntFilter $ \filter -> fmap fromMono . monoSig1 (C.dreamPad filter bright)

-- | The first argument is brightness (0 to 1). The second argument is a wave function.
dreamPadBym' :: Sig -> (Sig -> SE Sig) -> Patch2
dreamPadBym' bright wave = dreamPadFx $ monoSyntFilter $ \filter -> fmap fromMono . monoSig1 (C.dreamPadBy filter wave bright)

-- | The first argument is brightness (0 to 1)
underwaterPad' :: Sig -> Patch2
underwaterPad' bright = dreamPadFx $ polySyntFilter $ \filter ->  fmap fromMono . onCps (C.underwaterPad filter bright)

-- | The first argument is brightness (0 to 1)
underwaterPadm' :: Sig -> Patch2
underwaterPadm' bright = dreamPadFx $ monoSyntFilter $ \filter -> fmap fromMono . monoSig1 (C.underwaterPad filter bright)

-- | The first argument is brightness (0 to 1)
lightIsTooBrightPad' :: Sig -> Patch2
lightIsTooBrightPad' bright = dreamPadFx $ polySyntFilter $ \filter ->  fmap fromMono . onCps (C.lightIsTooBrightPad filter bright)

lightIsTooBrightPadm' :: Sig -> Patch2
lightIsTooBrightPadm' bright = dreamPadFx $ monoSyntFilter $ \filter -> fmap fromMono . monoSig1 (C.lightIsTooBrightPad filter bright)

lightIsTooBrightPadWithKey :: Sig -> (D -> Sig -> SE Sig) -> Patch2
lightIsTooBrightPadWithKey brightness wave = dreamPadFx $ polySyntFilter $ genDreamPadNote C.lightIsTooBrightPadBy wave brightness

-- | The first argument is brightness (0 to 1)
whaleSongPad' :: Sig -> Patch2
whaleSongPad' bright = dreamPadFx $ polySyntFilter $ \filter -> fmap fromMono . onCps (C.whaleSongPad filter bright)

whaleSongPadm' :: Sig -> Patch2
whaleSongPadm' bright = dreamPadFx $ monoSyntFilter $ \filter -> fmap fromMono . monoSig1 (C.whaleSongPad filter bright)

whaleSongPadWithKey :: Sig -> (D -> Sig -> SE Sig) -> Patch2
whaleSongPadWithKey brightness wave = dreamPadFx $ polySyntFilter $ genDreamPadNote C.whaleSongPadBy wave brightness

------------------------------------
-- leads

polySynthFxChain = FxChain [fxSpec 0.25 (return . largeHall2), fxSpec 0.25 (return . (at $ echo 0.25 0.65)), fxSpec 0.25 (at $ chorus 0.07 1.25 1), fxSpecFilter 1 $ \filter -> return . at (filter 5500 0.12 . filt 2 br 18000 0.3)]

polySynth  = polySynthFxChain $ polySynt $ fmap fromMono . onCps C.polySynth
polySynthm = polySynthFxChain $ monoSynt $ fmap fromMono . monoSig1 C.polySynth

phasingLead = withSmallHall $ polySynt $ at fromMono . mul (0.7 * fadeOut 0.05) . onCps (uncurry C.phasingSynth)

data RazorLead = RazorLead
	{ razorLeadBright :: Sig
	, razorLeadSpeed  :: Sig }

instance Default RazorLead where
	def = RazorLead 0.5 0.5

razorLeadSlow = razorLead' (def { razorLeadSpeed = 0.1 })
razorLeadFast = razorLead' (def { razorLeadSpeed = 1.7 })
razorLeadTremolo = razorLead' (def { razorLeadSpeed = 6.7 })

razorLead = razorLead' def

razorLead' (RazorLead bright speed) = fx1 0.35 smallHall2 $ polySynt $ at fromMono . (\(amp, cps) -> mul (fadeOut (0.05 + amp * 0.3)) $ C.razorLead (bright * sig amp) (speed * sig amp) (sig amp) (sig cps))

overtoneLeadFx :: Sig2 -> SE Sig2
overtoneLeadFx x = return $ magicCave2 $ mixAt 0.2 (echo 0.25 0.45) x

overtoneLead :: Patch2
overtoneLead = fx1' 0.15 overtoneLeadFx $ polySynt $ mul 0.4 . at fromMono . onCps (mul (fades 0.01 1) . C.tibetan 13 0.012)

------------------------------------
-- bass

simpleBass = withSmallRoom $ polySynt $ at fromMono . mul 0.32 . onCps C.simpleBass

pwBass = withSmallHall $ polySyntFilter $ \filter -> at fromMono . mul 0.4 . onCps (C.pwBassBy filter)

deepBass = withSmallHall $ polySynt $ at fromMono . mul 0.4 . onCps C.deepBass

fmBass1 = adsrMono (\env (amp, cps) -> return $ fromMono $ C.fmBass1 env (amp, cps))

fmBass2 = fxs $ adsrMono (\env (amp, cps) -> return $ fromMono $ C.fmBass2 env (amp, cps))
	where fxs = FxChain [fxSpec 1 (at (chorus 0.2 0.15 0.17)), fxSpec 1 (return . at (bhp 35 . blp 1200))]

-- | The first argument is the amount of deepBass to mix into the original patch.
withDeepBass :: Sig -> Patch2 -> Patch2
withDeepBass k = mixInstr k deepBass

------------------------------------
-- plucked

guitar = withSmallHall $ polySynt $ onCps $ fromMono . mul (0.6 * fades 0.01 0.25) . C.plainString

harpsichord = withSmallHall $ polySynt $ onCps $ fromMono . mul (0.65 * fades 0.01 0.13) . C.harpsichord

------------------------------------
-- strike

strikeFx :: Strike -> Sig2 -> SE Sig2
strikeFx spec a = at (strikeReverb spec) $ (if (strikeHasDelay spec) then (mixAt 0.35 (echo 0.25 0.55)) else id) (return a :: SE Sig2)

strikeRelease :: (D, D) -> Strike -> D
strikeRelease (amp, cps) spec = (0.85 * strikeRel spec * amp) * amp + (strikeRel spec) - (cps / 10000)

-- dac $ mixAt 0.15 largeHall2 $ mixAt 0.2 (echo 0.25 0.45) $ at fromMono $ midi $ onMsg $ onCps (mul (fadeOut 2) . C.tibetanBowl152 )
data Strike = Strike
	{ strikeRel :: D
	, strikeHasDelay ::	Bool
	, strikeReverb :: Sig2 -> Sig2
	}

instance Default Strike where
	def = Strike 1.5 True smallHall2

strike' :: Strike -> (Sig -> Sig) -> Patch2
strike' spec instr = fx1' 0.25 (strikeFx spec) $ polySynt $ \x@(amp, cps) -> return $ fromMono $ mul (0.75 * sig amp * fadeOut (rel x)) $ instr (sig cps)
	where rel a = strikeRelease a spec

data Size = Small | Medium | Large | Huge

nextSize x = case x of
	Small -> Medium
	Medium -> Large
	Large -> Huge
	Huge -> Huge

prevSize x = case x of
	Small -> Small
	Medium -> Small
	Large -> Medium
	Huge -> Large

toStrikeSpec :: Size -> Size -> Strike
toStrikeSpec revSpec restSpec = Strike
	{ strikeReverb  = toReverb revSpec
	, strikeRel = toRel restSpec
	, strikeHasDelay = toHasDelay restSpec }

toReverb :: Size -> (Sig2  -> Sig2)
toReverb x = case x of
	Small -> smallRoom2
	Medium -> smallHall2
	Large -> largeHall2
	Huge -> magicCave2

toRel :: Size -> D
toRel x = case x of
	Small -> 0.4
	Medium -> 1.5
	Large -> 2.5
	Huge -> 4.5

toGain :: Size -> Sig
toGain x = case x of
	Small -> 0.85
	Medium -> 0.75
	Large -> 0.6
	Huge -> 0.45

toHasDelay :: Size -> Bool
toHasDelay x = case x of
	Small -> False
	_     -> True

dahinaSize    		= Small
banyanSize    		= Medium
xylophoneSize 		= Small
tibetanBowl152Size  	= Medium
tibetanBowl140Size  	= Small
tibetanBowl180Size  	= Medium
spinelSphereSize    	= Small
potLidSize          	= Medium
redCedarWoodPlateSize = Small
tubularBellSize     	= Large
redwoodPlateSize    	= Small
douglasFirWoodPlateSize = Small
uniformWoodenBarSize = Small
uniformAluminumBarSize = Small
vibraphone1Size = Medium
vibraphone2Size = Medium
chalandiPlatesSize = Medium
wineGlassSize  = Medium
smallHandbellSize = Medium
albertClockBellBelfastSize = Large
woodBlockSize = Small

smallStrike :: Size -> (Sig -> Sig) -> Patch2
smallStrike size = mediumStrike' (prevSize size) size

mediumStrike :: Size -> (Sig -> Sig) -> Patch2
mediumStrike size = mediumStrike' size size

largeStrike :: Size -> (Sig -> Sig) -> Patch2
largeStrike size = mediumStrike' (nextSize size) size

magicStrike :: Size -> (Sig -> Sig) -> Patch2
magicStrike size = mediumStrike' (nextSize $ nextSize size) size

mediumStrike' :: Size -> Size -> (Sig -> Sig) -> Patch2
mediumStrike' revSize size f = mapPatchInstr (\instr -> mul (toGain size) . instr) p
	where p = strike' (toStrikeSpec revSize size) f


smallDahina = smallStrike dahinaSize C.dahina
dahina = mediumStrike dahinaSize C.dahina
largeDahina = largeStrike dahinaSize C.dahina
magicDahina = magicStrike dahinaSize C.dahina

smallBanyan = smallStrike banyanSize C.banyan
banyan = mediumStrike banyanSize C.banyan
largeBanyan = largeStrike banyanSize C.banyan
magicBanyan = magicStrike banyanSize C.banyan

smallXylophone = smallStrike xylophoneSize C.xylophone
xylophone = mediumStrike xylophoneSize C.xylophone
largeXylophone = largeStrike xylophoneSize C.xylophone
magicXylophone = magicStrike xylophoneSize C.xylophone

smallTibetanBowl180 = smallStrike tibetanBowl180Size C.tibetanBowl180
tibetanBowl180 = mediumStrike tibetanBowl180Size C.tibetanBowl180
largeTibetanBowl180 = largeStrike tibetanBowl180Size C.tibetanBowl180
magicTibetanBowl180 = magicStrike tibetanBowl180Size C.tibetanBowl180

smallSpinelSphere = smallStrike spinelSphereSize C.spinelSphere
spinelSphere = mediumStrike spinelSphereSize C.spinelSphere
largeSpinelSphere = largeStrike spinelSphereSize C.spinelSphere
magicSpinelSphere = magicStrike spinelSphereSize C.spinelSphere

smallPotLid = smallStrike potLidSize C.potLid
potLid = mediumStrike potLidSize C.potLid
largePotLid = largeStrike potLidSize C.potLid
magicPotLid = magicStrike potLidSize C.potLid

smallRedCedarWoodPlate = smallStrike redCedarWoodPlateSize C.redCedarWoodPlate
redCedarWoodPlate = mediumStrike redCedarWoodPlateSize C.redCedarWoodPlate
largeRedCedarWoodPlate = largeStrike redCedarWoodPlateSize C.redCedarWoodPlate
magicRedCedarWoodPlate = magicStrike redCedarWoodPlateSize C.redCedarWoodPlate

smallTubularBell = smallStrike tubularBellSize C.tubularBell
tubularBell = mediumStrike tubularBellSize C.tubularBell
largeTubularBell = largeStrike tubularBellSize C.tubularBell
magicTubularBell = magicStrike tubularBellSize C.tubularBell

smallRedwoodPlate = smallStrike redwoodPlateSize C.redwoodPlate
redwoodPlate = mediumStrike redwoodPlateSize C.redwoodPlate
largeRedwoodPlate = largeStrike redwoodPlateSize C.redwoodPlate
magicRedwoodPlate = magicStrike redwoodPlateSize C.redwoodPlate

smallDouglasFirWoodPlate = smallStrike douglasFirWoodPlateSize C.douglasFirWoodPlate
douglasFirWoodPlate = mediumStrike douglasFirWoodPlateSize C.douglasFirWoodPlate
largeDouglasFirWoodPlate = largeStrike douglasFirWoodPlateSize C.douglasFirWoodPlate
magicDouglasFirWoodPlate = magicStrike douglasFirWoodPlateSize C.douglasFirWoodPlate

smallUniformWoodenBar = smallStrike uniformWoodenBarSize C.uniformWoodenBar
uniformWoodenBar = mediumStrike uniformWoodenBarSize C.uniformWoodenBar
largeUniformWoodenBar = largeStrike uniformWoodenBarSize C.uniformWoodenBar
magicUniformWoodenBar = magicStrike uniformWoodenBarSize C.uniformWoodenBar

smallUniformAluminumBar = smallStrike uniformAluminumBarSize C.uniformAluminumBar
uniformAluminumBar = mediumStrike uniformAluminumBarSize C.uniformAluminumBar
largeUniformAluminumBar = largeStrike uniformAluminumBarSize C.uniformAluminumBar
magicUniformAluminumBar = magicStrike uniformAluminumBarSize C.uniformAluminumBar

smallVibraphone1 = smallStrike vibraphone1Size C.vibraphone1
vibraphone1 = mediumStrike vibraphone1Size C.vibraphone1
largeVibraphone1 = largeStrike vibraphone1Size C.vibraphone1
magicVibraphone1 = magicStrike vibraphone1Size C.vibraphone1

smallVibraphone2 = smallStrike vibraphone2Size C.vibraphone2
vibraphone2 = mediumStrike vibraphone2Size C.vibraphone2
largeVibraphone2 = largeStrike vibraphone2Size C.vibraphone2
magicVibraphone2 = magicStrike vibraphone2Size C.vibraphone2

smallChalandiPlates = smallStrike chalandiPlatesSize C.chalandiPlates
chalandiPlates = mediumStrike chalandiPlatesSize C.chalandiPlates
largeChalandiPlates = largeStrike chalandiPlatesSize C.chalandiPlates
magicChalandiPlates = magicStrike chalandiPlatesSize C.chalandiPlates

smallTibetanBowl152 = smallStrike tibetanBowl152Size C.tibetanBowl152
tibetanBowl152 = mediumStrike tibetanBowl152Size C.tibetanBowl152
largeTibetanBowl152 = largeStrike tibetanBowl152Size C.tibetanBowl152
magicTibetanBowl152 = magicStrike tibetanBowl152Size C.tibetanBowl152

smallTibetanBowl140 = smallStrike tibetanBowl140Size C.tibetanBowl140
tibetanBowl140 = mediumStrike tibetanBowl140Size C.tibetanBowl140
largeTibetanBowl140 = largeStrike tibetanBowl140Size C.tibetanBowl140
magicTibetanBowl140 = magicStrike tibetanBowl140Size C.tibetanBowl140

smallWineGlass = smallStrike wineGlassSize C.wineGlass
wineGlass = mediumStrike wineGlassSize C.wineGlass
largeWineGlass = largeStrike wineGlassSize C.wineGlass
magicWineGlass = magicStrike wineGlassSize C.wineGlass

smallHandbell = smallStrike smallHandbellSize C.smallHandbell
handbell = mediumStrike smallHandbellSize C.smallHandbell
largeHandbell = largeStrike smallHandbellSize C.smallHandbell
magicHandbell = magicStrike smallHandbellSize C.smallHandbell

smallAlbertClockBellBelfast = smallStrike albertClockBellBelfastSize C.albertClockBellBelfast
albertClockBellBelfast = mediumStrike albertClockBellBelfastSize C.albertClockBellBelfast
largeAlbertClockBellBelfast = largeStrike albertClockBellBelfastSize C.albertClockBellBelfast
magicAlbertClockBellBelfast = magicStrike albertClockBellBelfastSize C.albertClockBellBelfast

smallWoodBlock = smallStrike woodBlockSize C.woodBlock
woodBlock = mediumStrike woodBlockSize C.woodBlock
largeWoodBlock = largeStrike woodBlockSize C.woodBlock
magicWoodBlock = magicStrike woodBlockSize C.woodBlock

---------------------------------------------------------------
-- scrape

-- scrapePatch

names = ["dahina","banyan","xylophone","tibetanBowl180","spinelSphere","potLid","redCedarWoodPlate","tubularBell","redwoodPlate","douglasFirWoodPlate","uniformWoodenBar","uniformAluminumBar","vibraphone1","vibraphone2","chalandiPlates","tibetanBowl152","tibetanBowl140","wineGlass","smallHandbell","albertClockBellBelfast","woodBlock"]
toUpperName (x:xs) = toUpper x : xs

-- scrapePatch

scrapeRelease :: (D, D) -> D -> D
scrapeRelease (amp, cps) rel = (0.85 * rel * amp) * amp + rel - (cps / 10000)

scrapeFast k m = fx1 0.15 largeHall2 $ polySynt $ \x@(amp, cps) -> (mul (0.75 * sig amp * k * fades 0.02 (scrapeRelease x 0.25)) . at fromMono . C.scrapeModes m) (sig cps)

scrape k m = fx1 0.15 largeHall2 $ polySynt $ \x@(amp, cps) -> (mul (0.75 * sig amp * k * fades 0.5 (scrapeRelease x 0.97)) . at fromMono . C.scrapeModes m) (sig cps)

scrapem k m = fx1 0.15 largeHall2 $ monoSynt $ (\(amp, cps) -> (mul (0.75 * amp * k * fades 0.5 1.97) . at fromMono . C.scrapeModes m) cps) . monoArgToNote

scrapePad k m = fx1 0.15 largeHall2 $ polySynt $ \x@(amp, cps) -> (mul (0.75 * sig amp * k * fades 0.5 (scrapeRelease x 2.27	)) . at fromMono . C.scrapeModes m) (sig cps)

scrapePadm k m = fx1 0.15 largeHall2 $ monoSynt $ (\(amp, cps) -> (mul (0.75 * amp * k * fades 0.5 2.27) . at fromMono . C.scrapeModes m) cps) . monoArgToNote

scaleScrapeDahina = 1.32
scaleScrapeBanyan = 0.95
scaleScrapeXylophone = 1
scaleScrapeTibetanBowl180 = 0.55
scaleScrapeSpinelSphere = 1.4
scaleScrapePotLid = 0.65
scaleScrapeRedCedarWoodPlate = 1
scaleScrapeTubularBell = 0.75
scaleScrapeRedwoodPlate = 1
scaleScrapeDouglasFirWoodPlate = 1
scaleScrapeUniformWoodenBar = 1
scaleScrapeUniformAluminumBar = 0.75
scaleScrapeVibraphone1 = 0.9
scaleScrapeVibraphone2 = 0.9
scaleScrapeChalandiPlates = 1
scaleScrapeTibetanBowl152 = 0.65
scaleScrapeTibetanBowl140 = 0.75
scaleScrapeWineGlass = 0.6
scaleScrapeSmallHandbell = 1
scaleScrapeAlbertClockBellBelfast = 0.5
scaleScrapeWoodBlock = 1.32

scrapeDahina = scrape scaleScrapeDahina C.dahinaModes
scrapeBanyan = scrape scaleScrapeBanyan C.banyanModes
scrapeXylophone = scrape scaleScrapeXylophone C.xylophoneModes
scrapeTibetanBowl180 = scrape scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapeSpinelSphere = scrape scaleScrapeSpinelSphere C.spinelSphereModes
scrapePotLid = scrape scaleScrapePotLid C.potLidModes
scrapeRedCedarWoodPlate = scrape scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapeTubularBell = scrape scaleScrapeTubularBell C.tubularBellModes
scrapeRedwoodPlate = scrape scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapeDouglasFirWoodPlate = scrape scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapeUniformWoodenBar = scrape scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapeUniformAluminumBar = scrape scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapeVibraphone1 = scrape scaleScrapeVibraphone1 C.vibraphoneModes1
scrapeVibraphone2 = scrape scaleScrapeVibraphone2 C.vibraphoneModes2
scrapeChalandiPlates = scrape scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapeTibetanBowl152 = scrape scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapeTibetanBowl140 = scrape scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapeWineGlass = scrape scaleScrapeWineGlass C.wineGlassModes
scrapeSmallHandbell = scrape scaleScrapeSmallHandbell C.smallHandbellModes
scrapeAlbertClockBellBelfast = scrape scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapeWoodBlock = scrape scaleScrapeWoodBlock C.woodBlockModes

scrapeDahinam = scrapem scaleScrapeDahina C.dahinaModes
scrapeBanyanm = scrapem scaleScrapeBanyan C.banyanModes
scrapeXylophonem = scrapem scaleScrapeXylophone C.xylophoneModes
scrapeTibetanBowl180m = scrapem scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapeSpinelSpherem = scrapem scaleScrapeSpinelSphere C.spinelSphereModes
scrapePotLidm = scrape scaleScrapePotLid C.potLidModes
scrapeRedCedarWoodPlatem = scrapem scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapeTubularBellm = scrapem scaleScrapeTubularBell C.tubularBellModes
scrapeRedwoodPlatem = scrapem scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapeDouglasFirWoodPlatem = scrapem scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapeUniformWoodenBarm = scrapem scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapeUniformAluminumBarm = scrapem scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapeVibraphone1m = scrapem scaleScrapeVibraphone1 C.vibraphoneModes1
scrapeVibraphone2m = scrapem scaleScrapeVibraphone2 C.vibraphoneModes2
scrapeChalandiPlatesm = scrapem scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapeTibetanBowl152m = scrapem scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapeTibetanBowl140m = scrapem scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapeWineGlassm = scrapem scaleScrapeWineGlass C.wineGlassModes
scrapeSmallHandbellm = scrapem scaleScrapeSmallHandbell C.smallHandbellModes
scrapeAlbertClockBellBelfastm = scrapem scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapeWoodBlockm = scrapem scaleScrapeWoodBlock C.woodBlockModes

scrapeFastDahina = scrapeFast scaleScrapeDahina C.dahinaModes
scrapeFastBanyan = scrapeFast scaleScrapeBanyan C.banyanModes
scrapeFastXylophone = scrapeFast scaleScrapeXylophone C.xylophoneModes
scrapeFastTibetanBowl180 = scrapeFast scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapeFastSpinelSphere = scrapeFast scaleScrapeSpinelSphere C.spinelSphereModes
scrapeFastPotLid = scrapeFast scaleScrapePotLid C.potLidModes
scrapeFastRedCedarWoodPlate = scrapeFast scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapeFastTubularBell = scrapeFast scaleScrapeTubularBell C.tubularBellModes
scrapeFastRedwoodPlate = scrapeFast scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapeFastDouglasFirWoodPlate = scrapeFast scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapeFastUniformWoodenBar = scrapeFast scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapeFastUniformAluminumBar = scrapeFast scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapeFastVibraphone1 = scrapeFast scaleScrapeVibraphone1 C.vibraphoneModes1
scrapeFastVibraphone2 = scrapeFast scaleScrapeVibraphone2 C.vibraphoneModes2
scrapeFastChalandiPlates = scrapeFast scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapeFastTibetanBowl152 = scrapeFast scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapeFastTibetanBowl140 = scrapeFast scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapeFastWineGlass = scrapeFast scaleScrapeWineGlass C.wineGlassModes
scrapeFastSmallHandbell = scrapeFast scaleScrapeSmallHandbell C.smallHandbellModes
scrapeFastAlbertClockBellBelfast = scrapeFast scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapeFastWoodBlock = scrapeFast scaleScrapeWoodBlock C.woodBlockModes

scrapePadDahina = scrapePad scaleScrapeDahina C.dahinaModes
scrapePadBanyan = scrapePad scaleScrapeBanyan C.banyanModes
scrapePadXylophone = scrapePad scaleScrapeXylophone C.xylophoneModes
scrapePadTibetanBowl180 = scrapePad scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapePadSpinelSphere = scrapePad scaleScrapeSpinelSphere C.spinelSphereModes
scrapePadPotLid = scrapePad scaleScrapePotLid C.potLidModes
scrapePadRedCedarWoodPlate = scrapePad scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapePadTubularBell = scrapePad scaleScrapeTubularBell C.tubularBellModes
scrapePadRedwoodPlate = scrapePad scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapePadDouglasFirWoodPlate = scrapePad scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapePadUniformWoodenBar = scrapePad scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapePadUniformAluminumBar = scrapePad scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapePadVibraphone1 = scrapePad scaleScrapeVibraphone1 C.vibraphoneModes1
scrapePadVibraphone2 = scrapePad scaleScrapeVibraphone2 C.vibraphoneModes2
scrapePadChalandiPlates = scrapePad scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapePadTibetanBowl152 = scrapePad scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapePadTibetanBowl140 = scrapePad scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapePadWineGlass = scrapePad scaleScrapeWineGlass C.wineGlassModes
scrapePadSmallHandbell = scrapePad scaleScrapeSmallHandbell C.smallHandbellModes
scrapePadAlbertClockBellBelfast = scrapePad scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapePadWoodBlock = scrapePad scaleScrapeWoodBlock C.woodBlockModes

scrapePadDahinam = scrapePadm scaleScrapeDahina C.dahinaModes
scrapePadBanyanm = scrapePadm scaleScrapeBanyan C.banyanModes
scrapePadXylophonem = scrapePadm scaleScrapeXylophone C.xylophoneModes
scrapePadTibetanBowl180m = scrapePadm scaleScrapeTibetanBowl180 C.tibetanBowlModes180
scrapePadSpinelSpherem = scrapePadm scaleScrapeSpinelSphere C.spinelSphereModes
scrapePadPotLidm = scrapePadm scaleScrapePotLid C.potLidModes
scrapePadRedCedarWoodPlatem = scrapePadm scaleScrapeRedCedarWoodPlate C.redCedarWoodPlateModes
scrapePadTubularBellm = scrapePadm scaleScrapeTubularBell C.tubularBellModes
scrapePadRedwoodPlatem = scrapePadm scaleScrapeRedwoodPlate C.redwoodPlateModes
scrapePadDouglasFirWoodPlatem = scrapePadm scaleScrapeDouglasFirWoodPlate C.douglasFirWoodPlateModes
scrapePadUniformWoodenBarm = scrapePadm scaleScrapeUniformWoodenBar C.uniformWoodenBarModes
scrapePadUniformAluminumBarm = scrapePadm scaleScrapeUniformAluminumBar C.uniformAluminumBarModes
scrapePadVibraphone1m = scrapePadm scaleScrapeVibraphone1 C.vibraphoneModes1
scrapePadVibraphone2m = scrapePadm scaleScrapeVibraphone2 C.vibraphoneModes2
scrapePadChalandiPlatesm = scrapePadm scaleScrapeChalandiPlates C.chalandiPlatesModes
scrapePadTibetanBowl152m = scrapePadm scaleScrapeTibetanBowl152 C.tibetanBowlModes152
scrapePadTibetanBowl140m = scrapePadm scaleScrapeTibetanBowl140 C.tibetanBowlModes140
scrapePadWineGlassm = scrapePadm scaleScrapeWineGlass C.wineGlassModes
scrapePadSmallHandbellm = scrapePadm scaleScrapeSmallHandbell C.smallHandbellModes
scrapePadAlbertClockBellBelfastm = scrapePadm scaleScrapeAlbertClockBellBelfast C.albertClockBellBelfastModes
scrapePadWoodBlockm = scrapePadm scaleScrapeWoodBlock C.woodBlockModes


------------------------------------
-- woodwind

data Wind = Wind
	{ windAtt :: D
	, windDec :: D
	, windSus :: D
	, windVib :: D
	, windBright :: D }

woodWind' spec instr = withSmallHall $ polySynt $ \(amp, cps) -> mul 1.3 $ do
		seed <- rnd 1
		vibDisp <- rnd (0.1 * amp)
		let dispVib vib = vib * (0.9 + vibDisp)
		return $ fromMono $ mul (0.8 * sig amp * fadeOut (windDec spec)) $ instr seed (dispVib $ windVib spec) (windAtt spec) (windSus spec) (windDec spec) (0.4 + 0.75 * windBright spec * amp) cps

-- flute

fluteSpec bright vib = Wind
	{ windAtt = 0.08
	, windDec = 0.1
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortFluteSpec bright vib = Wind
	{ windAtt = 0.03
	, windDec = 0.05
	, windSus = 20
	, windVib = vib
	, windBright = bright }

flute = woodWind' (fluteSpec br vib) C.flute
	where
		br = 0.7
		vib = 0.015

shortFlute = woodWind' (shortFluteSpec br vib) C.flute
	where
		br = 0.7
		vib = 0.015

fluteVibrato = woodWind' (fluteSpec br vib) C.flute
	where
		br = 0.7
		vib = 0.04

mutedFlute = woodWind' (fluteSpec br vib) C.flute
	where
		br = 0.25
		vib = 0.015

brightFlute = woodWind' (fluteSpec br vib) C.flute
	where
		br = 1.2
		vib = 0.015

-- bass clarinet

bassClarinetSpec bright vib = Wind
	{ windAtt = 0.06
	, windDec = 0.15
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortBassClarinetSpec bright vib = Wind
	{ windAtt = 0.03
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

bassClarinet = woodWind' (bassClarinetSpec br vib) C.bassClarinet
	where
		br = 0.7
		vib = 0.01

shortBassClarinet = woodWind' (shortBassClarinetSpec br vib) C.bassClarinet
	where
		br = 0.7
		vib = 0.01

bassClarinetVibrato = woodWind' (bassClarinetSpec br vib) C.bassClarinet
	where
		br = 0.7
		vib = 0.035

mutedBassClarinet = woodWind' (bassClarinetSpec br vib) C.bassClarinet
	where
		br = 0.25
		vib = 0.01

brightBassClarinet = woodWind' (bassClarinetSpec br vib) C.bassClarinet
	where
		br = 1.2
		vib = 0.01

-- french horn

frenchHornSpec bright vib = Wind
	{ windAtt = 0.08
	, windDec = 0.25
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortFrenchHornSpec bright vib = Wind
	{ windAtt = 0.03
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

frenchHorn = woodWind' (frenchHornSpec br vib) C.frenchHorn
	where
		br = 0.7
		vib = 0.01

shortFrenchHorn = woodWind' (shortFrenchHornSpec br vib) C.frenchHorn
	where
		br = 0.7
		vib = 0.01

frenchHornVibrato = woodWind' (frenchHornSpec br vib) C.frenchHorn
	where
		br = 0.7
		vib = 0.035

mutedFrenchHorn = woodWind' (frenchHornSpec br vib) C.frenchHorn
	where
		br = 0.25
		vib = 0.01

brightFrenchHorn = woodWind' (frenchHornSpec br vib) C.frenchHorn
	where
		br = 1.2
		vib = 0.01

-- sheng

shengSpec bright vib = Wind
	{ windAtt = 0.1
	, windDec = 0.2
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortShengSpec bright vib = Wind
	{ windAtt = 0.03
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

sheng = woodWind' (shengSpec br vib) C.sheng
	where
		br = 0.7
		vib = 0.01

shortSheng = woodWind' (shortShengSpec br vib) C.sheng
	where
		br = 0.7
		vib = 0.01

shengVibrato = woodWind' (shengSpec br vib) C.sheng
	where
		br = 0.7
		vib = 0.025

mutedSheng = woodWind' (shengSpec br vib) C.sheng
	where
		br = 0.25
		vib = 0.01

brightSheng = woodWind' (shortShengSpec br vib) C.sheng
	where
		br = 1.2
		vib = 0.01

-- hulusi

hulusiSpec bright vib = Wind
	{ windAtt = 0.12
	, windDec = 0.14
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortHulusiSpec bright vib = Wind
	{ windAtt = 0.03
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

hulusi = woodWind' (hulusiSpec br vib) C.hulusi
	where
		br = 0.7
		vib = 0.015

shortHulusi = woodWind' (shortHulusiSpec br vib) C.hulusi
	where
		br = 0.7
		vib = 0.015

hulusiVibrato = woodWind' (hulusiSpec br vib) C.hulusi
	where
		br = 0.7
		vib = 0.035

mutedHulusi = woodWind' (hulusiSpec br vib) C.hulusi
	where
		br = 0.25
		vib = 0.015

brightHulusi = woodWind' (shortHulusiSpec br vib) C.hulusi
	where
		br = 1.2
		vib = 0.015


-- dizi

diziSpec bright vib = Wind
	{ windAtt = 0.03
	, windDec = 0.2
	, windSus = 20
	, windVib = vib
	, windBright = bright }

shortDiziSpec bright vib = Wind
	{ windAtt = 0.1
	, windDec = 0.04
	, windSus = 20
	, windVib = vib
	, windBright = bright }

dizi = woodWind' (diziSpec br vib) C.dizi
	where
		br = 0.7
		vib = 0.01

shortDizi = woodWind' (shortDiziSpec br vib) C.dizi
	where
		br = 0.7
		vib = 0.01

diziVibrato = woodWind' (diziSpec br vib) C.dizi
	where
		br = 0.7
		vib = 0.035

mutedDizi = woodWind' (diziSpec br vib) C.dizi
	where
		br = 0.25
		vib = 0.01

brightDizi = woodWind' (shortDiziSpec br vib) C.dizi
	where
		br = 1.2
		vib = 0.01

------------------------------------
-- x-rays

pulseWidth = fx1 0.15 smallHall2 $ polySynt $ mul (0.75 * 0.6) . at fromMono . mul (fades 0.07 0.1). onCps (uncurry C.pulseWidth)

xanadu = fx1 0.27 largeHall2 $polySynt $ mul (1.2 * 0.6) . at fromMono . mul (fades 0.01 2.2). onCps C.xanadu1

alienIsAngry = fx1 0.15 smallRoom2 $ polySynt $ at fromMono . mul (0.5 * fades 0.01 2.3). onCps (C.fmMod 5)

noiz = fx1 0.15 smallHall2 $ polySynt $ at fromMono . mul (1.5 * fades 0.01 0.5). onCps C.noiz

blue = fx1 0.25 smallHall2 $ polySynt $ at fromMono . mul (1.5 * fades 0.01 0.5). onCps (C.blue 5 7 0.24 12)

black = fx1 0.25 smallHall2 $ polySynt $ at fromMono . mul (2 * fades 0.01 0.5). onCps (\cps -> C.black 3 (cps / 2) (cps * 2) 12 (sig cps))

simpleMarimba = fx1 0.25 smallHall2 $ polySynt $ at fromMono . mul (0.8 * fades 0.01 0.5). onCps (C.simpleMarimba 5)

impulseMarimba1 = fx1 0.3 smallHall2 $ polySynt $ at fromMono . mul (0.8 * fadeOut 0.75). onCps C.impulseMarimba1

impulseMarimba2 = fx1 0.3 smallHall2 $ polySynt $ at fromMono . mul (0.8 * fadeOut 0.75). onCps C.impulseMarimba2

okComputer = polySyntFilter $ \filter (amp, cps) -> (at fromMono . mul (0.75 * sig amp * fades 0.01 0.01) . at (filter (1500 + sig amp * 8500) 0.1) . (C.okComputer . (/ 25))) (sig cps)

snowCrackle = polySynt $ \(amp, cps) -> (return . fromMono . mul (0.8 * sig amp * fades 0.001 0.001) . (C.snowCrackle . (/ 25))) (sig cps)

noiseBell = fx1 0.25 smallHall2 $ polySynt $ at fromMono . mul 0.75 . onCps (C.noiseBell (31, 125) 2.3 0.2 . ( * 8))

------------------------------------
-- vowels

robotVowels vows latVow = fx1 0.15 smallHall2 $ polySynt $ at fromMono . mul (1.1 * fades 0.1 0.1). onCps (C.vowels 25 vows latVow)

robotLoopVowels loopDur vows = fx1 0.15 smallHall2 $ polySynt $ at fromMono . mul (1.1 * fades 0.1 0.1). onCps (C.loopVowels 25 loopDur vows)

robotVowel vow = fx1 0.15 smallHall2 $ polySynt $ at fromMono . mul (1.1 * fades 0.1 0.1). onCps (C.oneVowel 25 vow)

------------------------------------
-- nature / effects

windWall = withLargeHall $ polySynt $ at fromMono . mul (1.25 * fades 0.1 5). onCps C.windWall

mildWind = withLargeHall $ polySynt $ at fromMono . mul (1.25 * fades 0.1 1.5). onCps C.mildWind

wind = withLargeHall $ polySynt $ at fromMono . mul (0.8 * fades 0.1 1.5). onCps (\cps -> C.thorWind (cps * 2) 150 (0.3, 1))

------------------------------------
-- drums

------------------------------------
-- SHARC patches

-- | Solo instrument.
soloSharc :: SharcInstr -> Patch2
soloSharc instr = withSmallHall $ polySynt $ fmap fromMono . onCps (C.soloSharcOsc instr)

-- | Instrumet played in ensemble (with chorus).
orcSharc :: SharcInstr -> Patch2
orcSharc instr = withLargeHall $ polySynt $ fmap fromMono . onCps (C.orcSharcOsc instr)

-- | Pad orchestra instrument.
padSharc :: SharcInstr -> Patch2
padSharc instr = withLargeHall $ polySynt $ fmap fromMono . onCps (C.padSharcOsc instr)

-- | Pad solo instrument.`
purePadSharc :: SharcInstr -> Patch2
purePadSharc instr = fx1 0.35 largeHall2 $ polySynt $ fmap fromMono . onCps (C.purePadSharcOsc instr)

-- | Dream Pad patch made with SHARC oscillators.
dreamSharc :: SharcInstr -> Patch2
dreamSharc instr = dreamPadWithKey 0.35 (C.rndSigSharcOsc instr)

-- | Dream Pad patch made with SHARC oscillators.
lightIsTooBrightSharc :: SharcInstr -> Patch2
lightIsTooBrightSharc instr = lightIsTooBrightPadWithKey 0.6 (C.rndSigSharcOsc instr)

-- | Dream Pad patch made with SHARC oscillators.
whaleSongSharc :: SharcInstr -> Patch2
whaleSongSharc instr = whaleSongPadWithKey 0.4 (C.rndSigSharcOsc instr)

sharcOrgan :: SharcInstr -> Patch2
sharcOrgan instr = waveOrganWithKey (C.rndSigSharcOsc instr)

type PadsynthBandwidth = Double

-- | Padsynth instrument with organ-like amplitude envelope.
psOrganSharc :: SharcInstr -> Patch2
psOrganSharc = psOrganSharc' def

hiDef = def { padSharcSize = 35 }

-- | High resolution Padsynth instrument with organ-like amplitude envelope.
psOrganSharcHifi :: SharcInstr -> Patch2
psOrganSharcHifi = psOrganSharc' hiDef

-- | Padsynth instrument with organ-like amplitude envelope. We can specify aux parameters.
psOrganSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psOrganSharc' spec sh = fxs $ polySynt $ mul (0.5 * fades 0.01 0.1) . onCps (C.padsynthSharcOsc2' spec sh)
    where fxs = FxChain [fxSpec 0.25 (return . smallHall2), fxSpec 1 (return . (at $ mul 1.4 . saturator 0.75))]

-- | Padsynth instrument with organ-like amplitude envelope and huge reverb.
psLargeOrganSharc :: SharcInstr -> Patch2
psLargeOrganSharc = psLargeOrganSharc' def

-- | High resolution Padsynth instrument with organ-like amplitude envelope and huge reverb.
psLargeOrganSharcHifi :: SharcInstr -> Patch2
psLargeOrganSharcHifi = psLargeOrganSharc' hiDef

-- | Padsynth instrument with organ-like amplitude envelope and huge reverb.
psLargeOrganSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psLargeOrganSharc' spec sh = fxs $ polySynt $ mul (0.65 * fades 0.01 0.1) . onCps (C.padsynthSharcOsc2' spec sh)
    where fxs = FxChain [fxSpec 0.35 (return . largeHall2), fxSpec 1 (return . (at $ mul 1.4 . saturator 0.75))]

-- | Padsynth instrument with piano-like amplitude envelope.
psPianoSharc :: ReleaseTime -> SharcInstr -> Patch2
psPianoSharc = psPianoSharc' def

-- | High resolution Padsynth instrument with piano-like amplitude envelope.
psPianoSharcHifi :: ReleaseTime -> SharcInstr -> Patch2
psPianoSharcHifi = psPianoSharc' hiDef

-- | Padsynth instrument with piano-like amplitude envelope. We can specify aux parameters.
psPianoSharc' :: PadSharcSpec -> ReleaseTime -> SharcInstr -> Patch2
psPianoSharc' spec releaseTime sh = fxs $ polySynt $ \ampCps -> mul (0.75 * C.pianoEnv releaseTime ampCps) $ onCps (C.padsynthSharcOsc2' spec sh) ampCps
    where fxs = FxChain [fxSpec 0.15 (return . smallHall2), fxSpec 1 (return . (at $ mul 1.4 . saturator 0.75))]

-- | Padsynth instrument with piano-like amplitude envelope.
xpsPianoSharc :: ReleaseTime -> SharcInstr -> Patch2
xpsPianoSharc = xpsPianoSharc' def

-- | High resolution Padsynth instrument with piano-like amplitude envelope.
xpsPianoSharcHifi :: ReleaseTime -> SharcInstr -> Patch2
xpsPianoSharcHifi = xpsPianoSharc' hiDef

-- | Padsynth instrument with piano-like amplitude envelope. We can specify aux parameters.
xpsPianoSharc' :: PadSharcSpec -> ReleaseTime -> SharcInstr -> Patch2
xpsPianoSharc' spec releaseTime sh = addHammer 0.12 $ fxs $ polySynt $ \ampCps -> mul (0.75 * C.xpianoEnv releaseTime ampCps) $ onCps (C.padsynthSharcOsc2' spec sh) ampCps
    where fxs = FxChain [fxSpec 0.15 (return . smallHall2), fxSpec 1 (return . (at $ mul 1.4 . saturator 0.75))]

-- | Padsynth instrument with piano-like amplitude envelope.
psLargePianoSharc :: ReleaseTime -> SharcInstr -> Patch2
psLargePianoSharc = psLargePianoSharc' def

-- | High resolution Padsynth instrument with piano-like amplitude envelope.
psLargePianoSharcHifi :: ReleaseTime -> SharcInstr -> Patch2
psLargePianoSharcHifi = psLargePianoSharc' hiDef

-- | Padsynth instrument with piano-like amplitude envelope. We can specify aux parameters.
psLargePianoSharc' :: PadSharcSpec -> ReleaseTime -> SharcInstr -> Patch2
psLargePianoSharc' spec releaseTime sh = fxs $ polySynt $ \ampCps -> mul (0.75 * C.pianoEnv releaseTime ampCps) $ onCps (C.padsynthSharcOsc2' spec sh) ampCps
    where fxs = FxChain [fxSpec 0.15 (return . largeHall2), fxSpec 1 (return . (at $ mul 1.4 . saturator 0.75))]

-- | Padsynth instrument with piano-like amplitude envelope.
xpsLargePianoSharc :: ReleaseTime -> SharcInstr -> Patch2
xpsLargePianoSharc = xpsLargePianoSharc' def

-- | High resolution Padsynth instrument with piano-like amplitude envelope.
xpsLargePianoSharcHifi :: ReleaseTime -> SharcInstr -> Patch2
xpsLargePianoSharcHifi = xpsLargePianoSharc' hiDef

-- | Padsynth instrument with piano-like amplitude envelope. We can specify aux parameters.
xpsLargePianoSharc' :: PadSharcSpec -> ReleaseTime -> SharcInstr -> Patch2
xpsLargePianoSharc' spec releaseTime sh = fxs $ polySynt $ \ampCps -> mul (0.75 * C.xpianoEnv releaseTime ampCps) $ onCps (C.padsynthSharcOsc2' spec sh) ampCps
    where fxs = FxChain [fxSpec 0.15 (return . largeHall2), fxSpec 1 (return . (at $ mul 1.4 . saturator 0.75))]

psPadFilterBy :: Sig -> Sig -> (Sig -> Sig -> Sig -> Sig) -> (D, D) -> Sig -> Sig
psPadFilterBy rippleLevel q resonFilter ampCps = resonFilter (0.3 * (sig $ snd ampCps) + 2500 + 2000 * fades 0.15 (0.6 + rel ampCps) + rippleLevel * slope 0.75 0.5 * osc 8) q
	where rel (amp, cps) = amp - cps / 3500

psPadFilter filter = psPadFilterBy 75 15 filter
psSoftPadFilter filter = psPadFilterBy 350 0.15 filter

deepOsc :: (Num a, SigSpace a) => (D -> a) -> (D -> a)
deepOsc f x = mul 0.5 (f x + f (x / 2))

psOsc spec sh x = C.padsynthSharcOsc2' spec sh x
psDeepOsc spec sh = deepOsc (C.padsynthSharcOsc2' spec sh)

psOscCfd koeff (spec1, sh1) (spec2, sh2) x = cfd koeff (C.padsynthSharcOsc2' spec1 sh1 x) (C.padsynthSharcOsc2' spec2 sh2 x)
psOscCfd4 koeffX koeffY (spec1, sh1) (spec2, sh2) (spec3, sh3) (spec4, sh4) x = cfd4 koeffX koeffY (C.padsynthSharcOsc2' spec1 sh1 x) (C.padsynthSharcOsc2' spec2 sh2 x) (C.padsynthSharcOsc2' spec3 sh3 x) (C.padsynthSharcOsc2' spec4 sh4 x)

psDeepOscCfd koeff (spec1, sh1) (spec2, sh2) = deepOsc (psOscCfd koeff (spec1, sh1) (spec2, sh2))
psDeepOscCfd4 koeffX koeffY (spec1, sh1) (spec2, sh2) (spec3, sh3) (spec4, sh4) = deepOsc (psOscCfd4 koeffX koeffY (spec1, sh1) (spec2, sh2) (spec3, sh3) (spec4, sh4))

genPsPad :: (Sig2 -> Sig2) -> (ResonFilter -> (D, D) -> Sig -> Sig) -> (D -> SE Sig2) -> Patch2
genPsPad effect mkFilter wave = fxs $ polySyntFilter $ \filter ampCps -> mul (1.2 * fades 0.5 (0.6 + rel ampCps)) $ onCps (at (mkFilter filter ampCps) . wave) ampCps
	where
        fxs = FxChain [fxSpec 0.25 (return . effect), fxSpec 0.5 (return . (at $ mul 2.1 . saturator 0.75)), fxSpec 0.3 (return . (at $ echo 0.125 0.65))]
        rel (amp, cps) = amp - cps / 3500

-- | Padsynth instrument with pad-like amplitude envelope.
psPadSharc :: SharcInstr -> Patch2
psPadSharc = psPadSharc' def

-- | High resolution Padsynth instrument with pad-like amplitude envelope.
psPadSharcHifi :: SharcInstr -> Patch2
psPadSharcHifi = psPadSharc' hiDef

-- | Padsynth instrument with pad-like amplitude envelope.
psPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psPadSharc' spec sh = genPsPad largeHall2 psPadFilter (psOsc spec sh)

-- | Padsynth instrument with pad-like amplitude envelope. Plays a note and one octave below it.
psDeepPadSharc :: SharcInstr -> Patch2
psDeepPadSharc = psDeepPadSharc' def

psDeepPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psDeepPadSharc' spec sh = genPsPad largeHall2 psPadFilter (psDeepOsc spec sh)

psPadSharcCfd :: Sig -> SharcInstr -> SharcInstr -> Patch2
psPadSharcCfd k sh1 sh2 = psPadSharcCfd' k (def, sh1) (def, sh2)

-- | Crossfade between timbres.
psPadSharcCfd' :: Sig -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> Patch2
psPadSharcCfd' k spec1 spec2 = genPsPad largeHall2 psPadFilter (psOscCfd k spec1 spec2)

psPadSharcCfd4 :: Sig -> Sig -> SharcInstr -> SharcInstr -> SharcInstr -> SharcInstr -> Patch2
psPadSharcCfd4 k1 k2 sh1 sh2 sh3 sh4 = psPadSharcCfd4' k1 k2 (def, sh1) (def, sh2) (def, sh3) (def, sh4)

-- | Crossfade between timbres.
psPadSharcCfd4' :: Sig -> Sig -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> Patch2
psPadSharcCfd4' k1 k2  spec1 spec2 spec3 spec4 = genPsPad largeHall2 psPadFilter (psOscCfd4 k1 k2 spec1 spec2 spec3 spec4)

psDeepPadSharcCfd :: Sig -> SharcInstr -> SharcInstr -> Patch2
psDeepPadSharcCfd k sh1 sh2 = psDeepPadSharcCfd' k (def, sh1) (def, sh2)

-- | Crossfade between timbres.
psDeepPadSharcCfd' :: Sig -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> Patch2
psDeepPadSharcCfd' k spec1 spec2 = genPsPad largeHall2 psPadFilter (psDeepOscCfd k spec1 spec2)

psDeepPadSharcCfd4 :: Sig -> Sig -> SharcInstr -> SharcInstr -> SharcInstr -> SharcInstr -> Patch2
psDeepPadSharcCfd4 k1 k2 sh1 sh2 sh3 sh4 = psDeepPadSharcCfd4' k1 k2 (def, sh1) (def, sh2) (def, sh3) (def, sh4)

-- | Crossfade between timbres.
psDeepPadSharcCfd4' :: Sig -> Sig -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> Patch2
psDeepPadSharcCfd4' k1 k2  spec1 spec2 spec3 spec4 = genPsPad largeHall2 psPadFilter (psDeepOscCfd4 k1 k2 spec1 spec2 spec3 spec4)


-- | Padsynth instrument with pad-like amplitude envelope and moog filter.
psSoftPadSharc :: SharcInstr -> Patch2
psSoftPadSharc = psSoftPadSharc' def

-- | High resolution Padsynth instrument with pad-like amplitude envelope and moog filter (resource hungry).
psSoftPadSharcHifi :: SharcInstr -> Patch2
psSoftPadSharcHifi = psSoftPadSharc' hiDef

-- | Padsynth instrument with pad-like amplitude envelope and moog filter.
-- We can specify aux parameters.
psSoftPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psSoftPadSharc' spec sh = genPsPad largeHall2 psSoftPadFilter (psOsc spec sh)

-- | Padsynth instrument with pad-like amplitude envelope and moog filter. Plays a note and one octave below it.
psDeepSoftPadSharc :: SharcInstr -> Patch2
psDeepSoftPadSharc = psDeepSoftPadSharc' def

psDeepSoftPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psDeepSoftPadSharc' spec sh = genPsPad largeHall2 psSoftPadFilter (psDeepOsc spec sh)


psSoftPadSharcCfd :: Sig -> SharcInstr -> SharcInstr -> Patch2
psSoftPadSharcCfd k sh1 sh2 = psSoftPadSharcCfd' k (def, sh1) (def, sh2)

-- | Crossfade between timbres.
psSoftPadSharcCfd' :: Sig -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> Patch2
psSoftPadSharcCfd' k spec1 spec2 = genPsPad largeHall2 psSoftPadFilter (psOscCfd k spec1 spec2)

psSoftPadSharcCfd4 :: Sig -> Sig -> SharcInstr -> SharcInstr -> SharcInstr -> SharcInstr -> Patch2
psSoftPadSharcCfd4 k1 k2 sh1 sh2 sh3 sh4 = psSoftPadSharcCfd4' k1 k2 (def, sh1) (def, sh2) (def, sh3) (def, sh4)

-- | Crossfade between timbres.
psSoftPadSharcCfd4' :: Sig -> Sig -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> Patch2
psSoftPadSharcCfd4' k1 k2  spec1 spec2 spec3 spec4 = genPsPad largeHall2 psSoftPadFilter (psOscCfd4 k1 k2 spec1 spec2 spec3 spec4)

psDeepSoftPadSharcCfd :: Sig -> SharcInstr -> SharcInstr -> Patch2
psDeepSoftPadSharcCfd k sh1 sh2 = psDeepSoftPadSharcCfd' k (def, sh1) (def, sh2)

-- | Crossfade between timbres.
psDeepSoftPadSharcCfd' :: Sig -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> Patch2
psDeepSoftPadSharcCfd' k spec1 spec2 = genPsPad largeHall2 psSoftPadFilter (psDeepOscCfd k spec1 spec2)

psDeepSoftPadSharcCfd4 :: Sig -> Sig -> SharcInstr -> SharcInstr -> SharcInstr -> SharcInstr -> Patch2
psDeepSoftPadSharcCfd4 k1 k2 sh1 sh2 sh3 sh4 = psDeepSoftPadSharcCfd4' k1 k2 (def, sh1) (def, sh2) (def, sh3) (def, sh4)

-- | Crossfade between timbres.
psDeepSoftPadSharcCfd4' :: Sig -> Sig -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> (PadSharcSpec, SharcInstr) -> Patch2
psDeepSoftPadSharcCfd4' k1 k2  spec1 spec2 spec3 spec4 = genPsPad largeHall2 psSoftPadFilter (psDeepOscCfd4 k1 k2 spec1 spec2 spec3 spec4)


-- | Padsynth instrument with pad-like amplitude envelope and @magicCave2@ reverb.
psMagicPadSharc :: SharcInstr -> Patch2
psMagicPadSharc = psMagicPadSharc' def

-- | High resolution Padsynth instrument with pad-like amplitude envelope and @magicCave2@ reverb.
psMagicPadSharcHifi :: SharcInstr -> Patch2
psMagicPadSharcHifi = psMagicPadSharc' hiDef

-- | Padsynth instrument with pad-like amplitude envelope and @magicCave2@ reverb.
psMagicPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psMagicPadSharc' spec sh = genPsPad magicCave2 psPadFilter (psOsc spec sh)

-- | Padsynth instrument with pad-like amplitude envelope and @magicCave2@ reverb. Plays a note and one octave below it.
psDeepMagicPadSharc :: SharcInstr -> Patch2
psDeepMagicPadSharc = psDeepMagicPadSharc' def

-- | Padsynth instrument with pad-like amplitude envelope and @magicCave2@ reverb. Plays a note and one octave below it.
psDeepMagicPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psDeepMagicPadSharc' spec sh = genPsPad magicCave2 psPadFilter (psDeepOsc spec sh)

-- | Padsynth instrument with pad-like amplitude envelope and moog filter and @magicCave2@ reverb (resource hungry).
psMagicSoftPadSharc :: SharcInstr -> Patch2
psMagicSoftPadSharc = psMagicSoftPadSharc' def

-- | High resolution Padsynth instrument with pad-like amplitude envelope and moog filter and @magicCave2@ reverb (resource hungry).
psMagicSoftPadSharcHifi :: SharcInstr -> Patch2
psMagicSoftPadSharcHifi = psMagicSoftPadSharc' hiDef

-- | Padsynth instrument with pad-like amplitude envelope and moog filter and @magicCave2@ reverb (resource hungry).
-- We can specify aux parameters.
psMagicSoftPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psMagicSoftPadSharc' spec sh = genPsPad magicCave2 psSoftPadFilter (psOsc spec sh)


-- | Padsynth instrument with pad-like amplitude envelope and moog filter and @magicCave2@ reverb (resource hungry).
psDeepMagicSoftPadSharc :: SharcInstr -> Patch2
psDeepMagicSoftPadSharc = psDeepMagicSoftPadSharc' def

-- | Padsynth instrument with pad-like amplitude envelope and moog filter and @magicCave2@ reverb (resource hungry).
-- We can specify aux parameters.
psDeepMagicSoftPadSharc' :: PadSharcSpec -> SharcInstr -> Patch2
psDeepMagicSoftPadSharc' spec sh = genPsPad magicCave2 psSoftPadFilter (psDeepOsc spec sh)

vedicSize = 15
vedicSizeHifi = 32
vedicSizeLofi = 4

-- | Deep spiritual drones.
--
-- > vedicPad sharcInstrument bandwidth
--
-- Good values for bandwidth lies in the interval [0, 120]
vedicPad :: SharcInstr -> PadsynthBandwidth -> Patch2
vedicPad instr bandwidth = mul 0.8 $
	addPreFx 0.45 (return . pingPong 0.25 0.65 0.5) $
	psDeepSoftPadSharc' (def { padSharcBandwidth = bandwidth, padSharcSize = 15 })  instr

-- | Deep spiritual drones. Crossfade between two instruments.
--
-- > vedicPadCfd cfdLevel sharcInstrument1 sharcInstrument2 bandwidth
--
-- Good values for bandwidth lies in the interval [0, 120]
vedicPadCfd :: Sig -> SharcInstr -> SharcInstr -> PadsynthBandwidth -> Patch2
vedicPadCfd k instr1 instr2 bandwidth = mul 0.8 $
	addPreFx 0.45 (return . pingPong 0.25 0.65 0.5) $
	psDeepSoftPadSharcCfd' k (def { padSharcBandwidth = bandwidth, padSharcSize = 15 },  instr1) (def { padSharcBandwidth = bandwidth, padSharcSize = 15 },  instr2)

-- | Deep spiritual drones. Crossfade between four instruments.
--
-- > vedicPadCfd4 cfdLevelX cfdLevelY sharcInstrument1 sharcInstrument2 sharcInstrument3 sharcInstrument4 bandwidth
--
-- Good values for bandwidth lies in the interval [0, 120]
vedicPadCfd4 :: Sig -> Sig -> SharcInstr -> SharcInstr -> SharcInstr -> SharcInstr -> PadsynthBandwidth -> Patch2
vedicPadCfd4 kX kY instr1 instr2 instr3 instr4 bandwidth = mul 0.8 $
	addPreFx 0.45 (return .  pingPong 0.25 0.65 0.5) $
	psDeepSoftPadSharcCfd4' kX kY
		(def { padSharcBandwidth = bandwidth, padSharcSize = 15 },  instr1) (def { padSharcBandwidth = bandwidth, padSharcSize = 15 },  instr2)
		(def { padSharcBandwidth = bandwidth, padSharcSize = 15 },  instr3) (def { padSharcBandwidth = bandwidth, padSharcSize = 15 },  instr4)


-- | Deep spiritual drones. Contains twice as many ftables as for simple @vedicPad@.
--
-- > vedicPad sharcInstrument bandwidth
--
-- Good values for bandwidth lies in the interval [0, 120]
vedicPadHifi :: SharcInstr -> PadsynthBandwidth -> Patch2
vedicPadHifi instr bandwidth = mul 0.8 $
	addPreFx 0.45 (return . pingPong 0.25 0.65 0.5) $
	deepPad $
	psSoftPadSharc' (def { padSharcBandwidth = bandwidth, padSharcSize = 32 })  instr

-- | Deep spiritual drones. Contains only quater of ftables as for simple @vedicPad@.
--
-- > vedicPad sharcInstrument bandwidth
--
-- Good values for bandwidth lies in the interval [0, 120]
vedicPadLofi :: SharcInstr -> PadsynthBandwidth -> Patch2
vedicPadLofi instr bandwidth = mul 0.8 $
	addPreFx 0.45 (return . pingPong 0.25 0.65 0.5) $
	deepPad $
	psSoftPadSharc' (def { padSharcBandwidth = bandwidth, padSharcSize = 4 })  instr

-- | Eminent
vibhu :: PadsynthBandwidth -> Patch2
vibhu = vedicPad shVibhu

shVibhu = shAltoFlute

-- | Wise
rishi :: PadsynthBandwidth -> Patch2
rishi = vedicPad shRishi

shRishi = shFlute

-- | Fire
agni :: PadsynthBandwidth -> Patch2
agni  =  vedicPad shAgni

shAgni = shCello

-- | Material nature
prakriti :: PadsynthBandwidth -> Patch2
prakriti = vedicPad shPrakriti

shPrakriti = shClarinet

-- | Desire
rajas :: PadsynthBandwidth -> Patch2
rajas = vedicPad shRajas

shRajas = shViolin

-- | the hero
avatara :: PadsynthBandwidth -> Patch2
avatara = vedicPad shAvatara

shAvatara = shFrenchHorn

-- | Earth
bhumi :: PadsynthBandwidth -> Patch2
bhumi = vedicPad shBhumi

shBhumi = shViolinsEnsemble

-- | Eminent
vibhuHifi :: PadsynthBandwidth -> Patch2
vibhuHifi = vedicPadHifi shAltoFlute

-- | Wise
rishiHifi :: PadsynthBandwidth -> Patch2
rishiHifi = vedicPadHifi shFlute

-- | Fire
agniHifi :: PadsynthBandwidth -> Patch2
agniHifi  =  vedicPadHifi shCello

-- | Material nature
prakritiHifi :: PadsynthBandwidth -> Patch2
prakritiHifi = vedicPadHifi shClarinet

-- | Desire
rajasHifi :: PadsynthBandwidth -> Patch2
rajasHifi = vedicPadHifi shViolin

-- | the hero
avataraHifi :: PadsynthBandwidth -> Patch2
avataraHifi = vedicPadHifi shFrenchHorn

-- | Earth
bhumiHifi :: PadsynthBandwidth -> Patch2
bhumiHifi = vedicPadHifi shViolinsEnsemble


-- | Eminent
vibhuLofi :: PadsynthBandwidth -> Patch2
vibhuLofi = vedicPadLofi shAltoFlute

-- | Wise
rishiLofi :: PadsynthBandwidth -> Patch2
rishiLofi = vedicPadLofi shFlute

-- | Fire
agniLofi :: PadsynthBandwidth -> Patch2
agniLofi  =  vedicPadLofi shCello

-- | Material nature
prakritiLofi :: PadsynthBandwidth -> Patch2
prakritiLofi = vedicPadLofi shClarinet

-- | Desire
rajasLofi :: PadsynthBandwidth -> Patch2
rajasLofi = vedicPadLofi shViolin

-- | the hero
avataraLofi :: PadsynthBandwidth -> Patch2
avataraLofi = vedicPadLofi shFrenchHorn

-- | Earth
bhumiLofi :: PadsynthBandwidth -> Patch2
bhumiLofi = vedicPadLofi shViolinsEnsemble

-----------------------------
-- crossfade pads

vedicCfd :: SharcInstr -> SharcInstr -> PadsynthBandwidth -> Sig -> Patch2
vedicCfd inst1 instr2 bandwidth cfdLevel = vedicPadCfd cfdLevel inst1 instr2 bandwidth

vibhuRishi :: PadsynthBandwidth -> Sig  -> Patch2
vibhuRishi = vedicCfd shVibhu shRishi

vibhuAgni :: PadsynthBandwidth -> Sig  -> Patch2
vibhuAgni = vedicCfd shVibhu shAgni

vibhuPrakriti :: PadsynthBandwidth -> Sig  -> Patch2
vibhuPrakriti = vedicCfd shVibhu shPrakriti

vibhuRajas :: PadsynthBandwidth -> Sig  -> Patch2
vibhuRajas = vedicCfd shVibhu shRajas

vibhuAvatara :: PadsynthBandwidth -> Sig  -> Patch2
vibhuAvatara = vedicCfd shVibhu shAvatara

vibhuBhumi :: PadsynthBandwidth -> Sig -> Patch2
vibhuBhumi = vedicCfd shVibhu shBhumi

rishiAgni :: PadsynthBandwidth -> Sig -> Patch2
rishiAgni = vedicCfd shRishi shAgni

rishiPrakriti :: PadsynthBandwidth -> Sig -> Patch2
rishiPrakriti = vedicCfd shRishi shPrakriti

rishiRajas :: PadsynthBandwidth -> Sig -> Patch2
rishiRajas = vedicCfd shRishi shRajas

rishiAvatara :: PadsynthBandwidth -> Sig -> Patch2
rishiAvatara = vedicCfd shRishi shAvatara

rishiBhumi :: PadsynthBandwidth -> Sig -> Patch2
rishiBhumi = vedicCfd shRishi shRajas

agniPrakriti :: PadsynthBandwidth -> Sig -> Patch2
agniPrakriti = vedicCfd shAgni shPrakriti

agniRajas :: PadsynthBandwidth -> Sig -> Patch2
agniRajas = vedicCfd shAgni shRajas

agniAvatara :: PadsynthBandwidth -> Sig -> Patch2
agniAvatara = vedicCfd shAgni shAvatara

agniBhumi :: PadsynthBandwidth -> Sig -> Patch2
agniBhumi = vedicCfd shAgni shBhumi

prakritiRajas :: PadsynthBandwidth -> Sig -> Patch2
prakritiRajas = vedicCfd shPrakriti shRajas

prakritiAvatara :: PadsynthBandwidth -> Sig -> Patch2
prakritiAvatara = vedicCfd shPrakriti shAvatara

prakritiBhumi :: PadsynthBandwidth -> Sig -> Patch2
prakritiBhumi = vedicCfd shPrakriti shBhumi

rajasAvatara :: PadsynthBandwidth -> Sig -> Patch2
rajasAvatara = vedicCfd shRajas shAvatara

rajasBhumi :: PadsynthBandwidth -> Sig -> Patch2
rajasBhumi = vedicCfd shRajas shBhumi

avataraBhumi :: PadsynthBandwidth -> Sig -> Patch2
avataraBhumi = vedicCfd shAvatara shBhumi

----------------------------------------
-- noisy padsynth pads

noisyRise :: Patch2
noisyRise = fxs $ polySynt $ onCps $ \cps -> mul 0.24 $ wave cps
	where
		fxs = FxChain [fxSpec 0.35 (return . largeHall2), fxSpec 0.5 (return . (at $ echo 0.25 0.85))]
		wave x  = noisy x + pad x
		noisy x = at (mul 0.3 . fromMono . bat (bp (x * 5) 23) . lp (300 + 2500 * linseg [0, 0.73, 0, 8, 3]) 14) white
		pad x = envelope $ filter x $ padsynthOsc2 spec x + mul 0.15 (padsynthOsc2 spec (x * 5)) + mul 0.5 (padsynthOsc2 spec (x / 2))

		envelope asig = mul (fades 0.5 0.7) asig
		filter cps asig = at (bhp 30) $ bat (lp (200 + (cps + 3000)) 45) $ asig

		spec = noisySpec

noisySpiral :: Patch2
noisySpiral = noisySpiral' 8

-- | Oscillating noise:
--
-- > noisySpiral' finalSpeedOfOscillation
noisySpiral' :: D -> Patch2
noisySpiral' spiralSpeed = fxs $ polySynt $ onCps $ \cps -> mul 0.24 $ wave cps
	where
		fxs = FxChain [fxSpec 0.15 (return . magicCave2), fxSpec 0.43 (return . (at $ echo 0.35 0.85))]

		wave x  = noisy x + pad x
		noisy x = at (mul 0.3 . fromMono . bat (bp (x * 5) 23) . lp (300 + 2500 * linseg [0, 0.73, 0, 8, 3] * uosc (expseg [0.25, 5, spiralSpeed])) 14) white
		pad x = envelope $ filter x $ padsynthOsc2 spec x + mul 0.15 (padsynthOsc2 spec (x * 5)) + mul 0.5 (padsynthOsc2 spec (x / 2))
		envelope asig = mul (fades 0.5 0.7) asig
		filter cps asig = at (bhp 30) $ bat (lp (200 + (cps + 3000)) 45) $ asig

		spec = noisySpec

noisyHarms = [ 1,  1, 0.7600046992, 0.6199994683, 0.9399998784, 0.4400023818, 0.0600003302, 0.8499968648, 0.0899999291, 0.8199964762, 0.3199984133, 0.9400014281, 0.3000001907, 0.120003365, 0.1799997687, 0.5200006366]
noisySpec  = defPadsynthSpec 82.2 noisyHarms


-- dac $ mul 0.24 $ at (bhp 30) $ mixAt 0.15 magicCave2 $ mixAt 0.43 (echo 0.35 0.85) $ midi $ onMsg $ (\cps -> (bat (lp (200 + (cps + 3000)) 45) . mul (fades 0.5 0.7) . (\x -> (at (mul 0.3 . fromMono . bat (bp (x * 11) 23) . lp (300 + 2500 * linseg [0, 0.73, 0, 8, 3] * uosc (expseg [0.25, 5, 8])) 14) white) +  padsynthOsc2 spec x + mul 0.15 (padsynthOsc2 spec (x * 5)) + mul 0.5 (padsynthOsc2 spec (x / 2)))) cps)

-- dac $ mul 0.24 $ at (bhp 30) $ mixAt 0.35 largeHall2 $ mixAt 0.5 (echo 0.25 0.85) $ midi $ onMsg $ (\cps -> (bat (lp (200 + (cps + 3000)) 45) . mul (fades 0.5 0.7) . (\x -> (at (mul 0.3 . fromMono . bat (bp (x * 5) 23) . lp (300 + 2500 * linseg [0, 0.73, 0, 8, 3]) 14) white) +  padsynthOsc2 spec x + mul 0.15 (padsynthOsc2 spec (x * 5)) + mul 0.5 (padsynthOsc2 spec (x / 2)))) cps)


----------------------------------

dafunkWave cfq adsrFun (amp, cps) = at (bhp 30) $ diode 1.2 (550 + 4500 * cfq) (0.52 + 0.4 * cfq) $ amp * env * (\x -> saw x + 0.5 * saw (x * 0.503) + 0.25 * (sqr (x * 0.253))) (port cps 0.001)
    where
        env = adsrFun 0.019 8.5 0.2 0.07

dafunkLead = adsrMono (\env (amp, cps) -> return $ fromMono $ dafunkWave cfq env (amp, cps))
    where cfq = uoscBy (sines [1, 0, 0, 0, 0.05]) 0.5


celloSynt :: Patch2
celloSynt = withSmallHall' 0.25 $ polySynt $ \(amp, cps) -> at fromMono $ C.celloWave (amp, sig cps)
