-- |  Drums of the Korg Mini Pops 7 drum machine (recoded from 	Iain McCurdy).
module Csound.Catalog.Drum.MiniPops(
	MpSpec(..),

	bass, snare1, snare2, rimShot, cymbal1, cymbal2, bongo1, bongo2, bongo3,
	claves, cowbell, guiro, maracas, quijada, tamb,

	-- * Generic
	bass', bdSpec, snare1', snSpec1, snare2', snSpec2, rimShot', rimSpec,
	cymbal1', cymSpec1, cymbal2', cymSpec2, bongo1', bonSpec1, bongo2', bonSpec2, bongo3', bonSpec3,
	claves', clSpec, cowbell', cowSpec, guiro', groSpec, maracas', marSpec, quijada', qjSpec, tamb', tamSpec,

	-- * Sample
	bd, sn1, sn2, rim, cym1, cym2, bon1, bon2, bon3, cl, cow, gro, mar, qj, tam,

	-- ** Generic
	bd', sn1', sn2', rim', cym1', cym2', bon1', bon2', bon3', cl', cow', gro', mar', qj', tam'
) where

import Csound.Base hiding (guiro)
import Csound.Sam

data MpSpec = MpSpec {
	  mpDur 	:: D
	, mpCps 	:: D
	, mpRnd     :: Maybe D }


rndAmp :: Sig -> SE Sig
rndAmp a = do
	k <- birnd 0.09
	return $ a * (1 + sig k)

addDur' dt x = xtratim dt >> return x
addDur = addDur' 0.1

toDrum :: Sig -> SE Sig
toDrum a = rndAmp =<< addDur a

defSpec dur cps = MpSpec
	{ mpDur   = dur
	, mpCps   = cps
	, mpRnd   = Just 0.085 }

rndVal :: D -> D -> D -> SE D
rndVal total amount x = do
	k <- birnd amount
	return $ x  + k * total

rndDur amt x = rndVal x amt x
rndCps amt x = rndVal x (amt / 10) x

rndSpec :: MpSpec -> SE MpSpec
rndSpec spec = do
	dur  <- rndDur'
	cps  <- rndCps'
	return $ spec
		{ mpDur  = dur
		, mpCps  = cps }
	where
		rndDur'  = (maybe return rndDur $ (mpRnd spec)) $ mpDur spec
		rndCps'  = (maybe return rndCps $ (mpRnd spec)) $ mpCps spec

rezz cps bw = reson (mpulse 1 0) cps (cps * bw) `withD` 2

bass = bass' bdSpec

bdSpec = defSpec 0.43 64

bass' spec = pureBass' =<< rndSpec spec

-- dur = 1.7
-- cps = 64
pureBass' spec = toDrum aout
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		aout = mul (env * 225 * fadeOut dur) $ lp1 500 $ rezz cps 0.001
		env  = transeg [1, dur, -14, 0]


snare1 = snare1' snSpec1

snSpec1 = defSpec 0.38 800

snare1' spec = pureSnare1' =<< rndSpec spec

-- cps = 800
pureSnare1' spec = toDrum =<< (mul (fadeOut dur) $ aout)
	where
		dur = mpDur spec
		cps = mpCps spec

		anoise = pink
		asig   = fmap (\x -> reson x 6250 9000 `withD` 1) anoise
		aenv   = transeg [1, dur ,-5 , 0]
		asig1  = at (bhp 3000) $ mul aenv asig

		xdur   = 0.006
		astrike = osc (transeg [cps,xdur,-4,60])
		aenv2 = transeg	[1,xdur,-2,0]
		astrike1 = aenv2 * astrike

		aout = fmap ((0.7 * astrike1) + ) $ mul 2 $ asig1

snare2 = snare2' snSpec2

snSpec2 = defSpec 0.4 800

snare2' spec = pureSnare2' =<< rndSpec spec

pureSnare2' spec = toDrum =<< (mul (fadeOut dur) $ aout)
	where
		dur = mpDur spec
		cps = mpCps spec

		anoise = pink
		asig   = fmap (\x -> butbp x 5200 5200 `withD` 1) anoise
		aenv   = transeg [1, dur ,-8 , 0]
		asig1  = at (bhp 3000) $ mul aenv asig

		xdur   = 0.005
		astrike = osc (transeg [cps,xdur,-4,cps / 4])
		aenv2 = transeg	[1,xdur,-2,0]
		astrike1 = aenv2 * astrike

		aout = fmap ((0.5 * astrike1) + ) $ mul 2.3 $ asig1


rimShot = rimShot' rimSpec

rimSpec = defSpec 0.005 1700

rimShot' spec = pureRimShot' =<< rndSpec spec

-- cps = 1700
-- dur = 0.005
pureRimShot' spec = toDrum $ mul (fadeOut dur) $ asig
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		aenv = expon 1 dur 0.0001
		asig1 = osc' 0.2 cps
		asig2 = reson asig1 cps 1500 `withD` 2
		asig  = bhp 500 (asig1 + asig2 * 0.4 * 0.3)

cymbal1 = cymbal1' cymSpec1

cymSpec1 = defSpec 0.304 6000

cymbal1' spec = pureCymbal1' =<< rndSpec spec

-- dur = 0.304
-- cps = 6000
pureCymbal1' spec = (toDrum =<< ) $ mul (fadeOut dur) $ do
	anoise <- white
	let asig1 = blp 14000 $ reson	(anoise*aenv) icf (icf*0.7) `withD` 1
	    asig2 = bhp 6000 $ (asig1 + anoise * 0.001)
	return $ 0.25 * aenv * asig2
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		aenv = transeg	[1,dur,-2,0]
		icf = cps


cymbal2 = cymbal2' cymSpec2

cymSpec2 = defSpec 1.404 1000

cymbal2' spec = pureCymbal2' =<< rndSpec spec

pureCymbal2' spec = (toDrum =<< ) $ mul (fadeOut dur) $ do
	anoise <- white
	let asig = mul aenv $ bhp 6000 $ mul aenv $ lp1 12000 $ reson (anoise * aenv) icf (icf * 0.9) `withD` 1
	return $ astrike * 0.2 + asig * 1.5
	where
		dur = mpDur spec
		cps = mpCps spec

		icf = sig $ cps * 5
		aenv = transeg	[1,dur,-2,0]
		xdur = 0.004
		aenv2 = transeg	[1,xdur,-2,0]
		astrike = mul aenv2 $ osc (transeg	[cps,xdur,-4,0.4*cps])

-- dur = 0.2
-- cps = 630
bongo1 = bongo1' bonSpec1

bonSpec1 = defSpec 0.2 630

bongo1' spec = pureBongo1' =<< rndSpec spec

pureBongo1' spec = toDrum $ mul (fadeOut dur) $ asig
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		asig = mul (4 * aenv ) $ blp 8000 $ bhp 300 $ rezz cps 0.03
		aenv = transeg	[1,dur,13,0]

bongo2 = bongo2' bonSpec2

bonSpec2 = defSpec 0.2 400

bongo2' spec = pureBongo2' =<< rndSpec spec

-- dur = 0.2
-- cps = 400
pureBongo2' spec = toDrum $ mul (fadeOut dur) $ asig
	where
		dur = mpDur spec
		cps = mpCps spec

		kcps =	expon	cps dur (cps * 0.975)
		aenv =	transeg	[1,dur-0.005,0,0.1,0.005,0, 0]
		asig = mul (4 * aenv) $ bhp 100 $ lp1 5000 $ rezz kcps 0.03


bongo3 = bongo3' bonSpec3

bonSpec3 = defSpec 1.229 194

bongo3' spec = pureBongo3' =<< rndSpec spec

-- dur = 1.229
-- cps = 194
pureBongo3' spec = toDrum $ mul (fadeOut dur) $ asig
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		aenv  =	transeg	[0, 0.001, -2, 1, dur-0.001, -2, 0]
		kbw   = linseg	[0.05,0.01,0.008]
		asig  = mul (5 * aenv) $ blp 11000 $ rezz cps kbw

claves = claves' clSpec

clSpec = defSpec 0.186 400

claves' spec = pureClaves' =<< rndSpec spec

pureClaves' spec = toDrum aout
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		aenv = linseg [1, dur, 0]
		asig1 = rezz cps 0.025
		asig2 = rezz (cps * 5.45) 0.03
		aout  = mul (3.2 * aenv * fadeOut dur) $ asig1 + 1.3 * asig2

cowbell = cowbell' cowSpec

cowSpec = defSpec 0.3 850

cowbell' spec = pureCowbell' =<< rndSpec spec

pureCowbell' spec = toDrum asig
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		asig = mul (aenv * 3 * fadeOut dur) $ bhp 100 $
			  rezz cps 0.007
			+ 0.8 * rezz (cps * 5.537) 0.03
		aenv = linseg [1, dur, 0]

guiro = guiro' groSpec

groSpec = defSpec 0.256 66

guiro' spec = pureGuiro' =<< rndSpec spec

pureGuiro' spec = toDrum asig
	where
		dur = mpDur spec
		cps = mpCps spec

		aenv =	linseg	[0,0.001,1,dur-0.111,0.6,0.1,1,0.01,0]
		asig = mul (3 * aenv * fadeOut dur) $ bhp 1000 $ reson (0.1 * sqr kcps) 4300 3000 `withD` 1
		kcps =	transeg	[cps,dur,2,(1.1 * cps)]

maracas = maracas' marSpec

marSpec = defSpec 0.05 5000

maracas' spec = pureMaracas' =<< rndSpec spec

pureMaracas' spec = toDrum =<< do
	asig <- noise 1 0.04
	return $ mul (0.35 * aenv * fadeOut dur) $ bhp 2000 $ reson asig 9000 4000 `withD` 2
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		aenv =	transeg	[1,dur,-4,0]


quijada = quijada' qjSpec

qjSpec = defSpec 0.817 550

quijada' spec = pureQuijada' =<< rndSpec spec

pureQuijada' spec = toDrum $ bhp cps $ mul (6 * fadeOut dur) $ phi dur (1/22.7272) + phi (dur * 0.39) (1/13.1579)
	where
		dur = mpDur spec
		cps = sig $ mpCps spec

		phi dt freq = mul kenv $ reson (mpulse	1 freq) 2727 400 `withD` 1
			where kenv = transeg	[0.8,0.05,1, 1,dt-0.05,-6,0]


tamb = tamb' tamSpec

tamSpec = defSpec 0.271 7000

tamb' spec = pureTamb' =<< rndSpec spec

pureTamb' spec = toDrum =<< do
	anoise <- noise 1 0
	return $ mul (1.5 * aenv * fadeOut dur)
		$ reson (bhp cps $ (+ (anoise * 0.1 * aenv)) $ reson (anoise * aenv) 4600 100 `withD` 2) 9000 3000 `withD` 1
	where
		dur = mpDur spec
		cps = sig $ mpCps spec
		aenv = transeg	[1,dur,-8,0]

-------------------------------------------------------
-- Sampler

mkSam = limSam 4

-- | Bass drum
bd :: Sam
bd = mkSam bass

-- | Snare 1
sn1 :: Sam
sn1 = mkSam snare1

-- | Snare 2
sn2 :: Sam
sn2 = limSam 2 snare2

-- | Rim shot
rim :: Sam
rim = limSam 1 rimShot

-- | Cymbal 1
cym1 :: Sam
cym1 = mkSam cymbal1

-- | Cymbal 2
cym2 :: Sam
cym2 = mkSam cymbal2

-- | Bongo 1
bon1 :: Sam
bon1 = mkSam bongo1

-- | Bongo 2
bon2 :: Sam
bon2 = mkSam bongo2

-- | Bongo 3
bon3 :: Sam
bon3 = mkSam bongo3

-- | Claves
cl :: Sam
cl = mkSam claves

-- | Cowbell
cow :: Sam
cow = mkSam cowbell

-- | Guiro
gro :: Sam
gro = mkSam guiro

-- | Maracas
mar :: Sam
mar = mkSam maracas

-- | Quijada
qj :: Sam
qj = mkSam quijada

-- | Tambourine
tam :: Sam
tam = mkSam tamb

mkSam' f spec = mkSam $ f spec

bd' :: MpSpec -> Sam
bd' = mkSam' bass'

sn1' :: MpSpec -> Sam
sn1' = mkSam' snare1'

sn2' :: MpSpec -> Sam
sn2' = mkSam' snare2'

rim' :: MpSpec -> Sam
rim' = mkSam' rimShot'

cym1' :: MpSpec -> Sam
cym1' = mkSam' cymbal1'

cym2' :: MpSpec -> Sam
cym2' = mkSam' cymbal2'

bon1' :: MpSpec -> Sam
bon1' = mkSam' bongo1'

bon2' :: MpSpec -> Sam
bon2' = mkSam' bongo2'

bon3' :: MpSpec -> Sam
bon3' = mkSam' bongo3'

cl' :: MpSpec -> Sam
cl' = mkSam' claves'

cow' :: MpSpec -> Sam
cow' = mkSam' cowbell'

gro' :: MpSpec -> Sam
gro' = mkSam' guiro'

mar' :: MpSpec -> Sam
mar' = mkSam' maracas'

qj' :: MpSpec -> Sam
qj' = mkSam' quijada'

tam' :: MpSpec -> Sam
tam' = mkSam' tamb'