{-# Language FlexibleContexts, ScopedTypeVariables #-}
-- | A multitap looper.
module Csound.Air.Looper (
	LoopSpec(..), LoopControl(..),
	sigLoop, midiLoop, sfLoop, patchLoop
) where

import Control.Monad
import Data.List

import Data.Default
import Data.Boolean
import Csound.Typed 
import Csound.Typed.Gui hiding (button)
import Csound.Control.Evt
import Csound.Control.Instr
import Csound.Control.Gui 
import Csound.Control.Sf

import Csound.Typed.Opcode hiding (space, button)
import Csound.SigSpace
import Csound.Air.Live	
import Csound.Air.Wave
import Csound.Air.Fx
import Csound.Air.Filter
import Csound.Air.Patch
import Csound.Air.Misc


-- | The type for fine tuning of the looper. Let's review the values:
-- 
-- * @loopMixVal@ - list of initial values for mix levels (default is 0.5 for all taps)
--
-- * @loopPrefx@ - list of pre-loop effects (the default is do-nothing effect)
--
-- * @loopPostfx@ - list of post-loop effects (the default is do-nothing effect)
--
-- * @loopPrefxVal@ - list of dry/wet values for pre-looop effects (the default is 0.5 for all taps)
--
-- * @loopPostfxVal@ - list of dry/wet values for post-looop effects (the default is 0.5 for all taps)
--
-- * @loopInitInstr@  - the initial sounding tap (sound source) (what tap we are going to record when the looper starts up).
--
-- * @loopFades@ - the list of instrument groups to fade/out. Eachl list item is a list of integers
-- where an integer points to a tap number. By default a single fader is given to each tap.
-- with lists of integers we can group the sound sources by their functions in the song.
-- We may group all harmonic instruments in a single group and all drums into another group.
--
-- * @loopReeatFades@ -- a repeat fade weight is a value that represents 
--    an amount of repetition. A looping tap is implemented as a delay tap with
--   big feedback. The repeat fades equals to the feedback amount. It have to be not bigger
-- 	 than 1. If the value equals to 1 than the loop is repeated forever. If it's lower
--   than 1 the loop is gradually going to fade. 
--
-- * @loopControl@ -- specifies an external controllers for the looper.
--   See the docs for the type @LoopSpec@.
data LoopSpec = LoopSpec 
	{ loopMixVal  :: [Sig]
	, loopPrefx  :: [FxFun]
	, loopPostfx :: [FxFun]
	, loopPrefxVal :: [Sig]
	, loopPostfxVal :: [Sig]	
	, loopInitInstr :: Int
	, loopFades :: [[Int]]	
	, loopRepeatFades :: [Sig]
	, loopControl :: LoopControl
	}

instance Default LoopSpec where
	def = LoopSpec {
		  loopPrefx  		= []
		, loopPostfx 		= []
		, loopPrefxVal 		= []
		, loopPostfxVal 	= []
		, loopMixVal      	= []
		, loopInitInstr 	= 0
		, loopFades 		= []
		, loopRepeatFades   = []
		, loopControl       = def
		}		

-- | External controllers. We can control the looper with
-- UI-widgets but sometimes it's convenient to control the
-- loper with some external midi-device. This structure mocks
-- all controls (except knobs for effects and mix).
--
-- * @loopTap@ - selects the current tap. It's a stream of integers (from 0 to a given integer).
--
-- * @loopFade@ - can fade in or fade out a group of taps. It's a list of toggle-like event streams.
--   they produce 1s for on and 0s for off.
--
-- * @loopDel@ - is for deleting the content of a given tap. It's just a click of the button.
--   So the value should be an event stream of units (which is @Tick = Evt Unit@).
--
-- * @loopThrough@ - is an event stream of toggles.
--
-- All values are wrapped in the @Maybe@ type. If the value is @Nothing@ in the given cell
-- the looper is controled only with virtual widgets.
--
-- There is an instance of @Default@ for @LoopControl@ with all values set to @Nothing@.
-- It's useful when we want to control only  a part of parameters externally.
-- We can use the value @def@ to set the  rest parameters:
--
-- > def { loopTap = Just someEvt }
data LoopControl = LoopControl 
	{ loopTap  :: Maybe (Evt D)
	, loopFade :: Maybe ([Evt D])
	, loopDel  :: Maybe Tick
	, loopThrough :: Maybe (Evt D)
	}

instance Default LoopControl where
	def = LoopControl {
		  loopTap  = Nothing
		, loopFade = Nothing
		, loopDel  = Nothing
		, loopThrough = Nothing }

type TapControl     = [String] -> Int -> Source Sig
type FadeControl    = [String -> Source (Evt D)]
type DelControl     = Source Tick
type ThroughControl = Source Sig

-- | The @midiLoop@ that is adapted for usage with soundfonts.
-- It takes in a list of pairs of sound fonts as sound sources.
-- The second value in the pair is the release time for the given sound font.
sfLoop :: LoopSpec -> D -> [D] -> [(Sf, D)] -> Source Sig2
sfLoop spec dtBpm times fonts = midiLoop spec dtBpm times $ fmap (uncurry sfMsg) fonts

-- | The @sigLoop@ that is adapted for usage with midi instruments.
-- It takes a list of midi instruments in place of signal inputs. The rest is the same
midiLoop :: LoopSpec -> D -> [D] -> [Msg -> SE Sig2] -> Source Sig2
midiLoop = genLoop $ \cond midiInstr -> midi $ playWhen cond midiInstr 

-- | Some instruments not work well with the looper. Alwo be aware of limitation of software resources.
patchLoop :: LoopSpec -> D -> [D] -> [Patch2] -> Source Sig2
patchLoop = genLoop $ \cond p -> atMidi (patchWhen cond p)

-- | Simple multitap Looper. We can create as many taps as we like
-- also we can create fade outs/ins insert effects and control mix. 
--
-- > sigLoop spec bpm times imputs 
--
-- Arguments:
--
-- * looper @spec@ (see the docs for the type)
--
-- * main @bpm@ rate. All taps are aligned with the main rate
--
-- * list of multipliers for each tap. Each tap is going to have a fixed
--    length that is a multiplier of the main rate. It doesn't have to be
--    an integer. So we can create weird drum patterns with odd loop durations.
--
-- * list of signal sources. By convention all sources are stereo signals.
--    We can use the function @fromMono@ to convert the mono signal to stereo.
sigLoop :: LoopSpec -> D -> [D] -> [Sig2] -> Source Sig2
sigLoop = genLoop $ \cond asig -> return $ mul (ifB cond 1 0) asig

getControls :: LoopControl -> (TapControl, FadeControl, DelControl, ThroughControl)
getControls a =	
	( maybe hradioSig (hradioSig' . evtToSig (-1)) (loopTap a)
	, fmap (\f x -> f x True) $ maybe (repeat toggle) (\xs -> fmap toggle' xs ++ repeat toggle) (loopFade a)
	, ( $ "del") $ maybe button button' (loopDel a)
	, (\f -> f "through" False) $ maybe toggleSig (toggleSig' . evtToSig (-1))  (loopThrough a)) 

genLoop :: forall a. (BoolSig -> a -> SE Sig2) -> LoopSpec -> D -> [D] -> [a] -> Source Sig2
genLoop playInstr spec dtBpm times' instrs = do
	(preFxKnobGui, preFxKnobWrite, preFxKnobRead) <- setKnob "pre" (linSpan 0 1) 0.5
	(postFxKnobGui, postFxKnobWrite, postFxKnobRead) <- setKnob "post" (linSpan 0 1) 0.5
	(mixKnobGui, mixKnobWrite, mixKnobRead) <- setKnob "mix" (linSpan 0 1) 0.5

	let knobGuis = ver [mixKnobGui, preFxKnobGui, postFxKnobGui]

	mapGuiSource (\gs -> hor [knobGuis, sca 12 gs]) $ joinSource $ vlift3 (\(thr, delEvt) x sils -> do
		-- knobs	
		mixCoeffs <- tabSigs mixKnobWrite mixKnobRead x initMixVals
		preCoeffs <- tabSigs preFxKnobWrite preFxKnobRead x initPreVals
		postCoeffs <- tabSigs postFxKnobWrite postFxKnobRead x initPostVals

		refs <- mapM (const $ newRef (1 :: Sig)) ids
		delRefs <- mapM (const $ newRef (0 :: Sig)) ids
		zipWithM_ (setSilencer refs) silencer sils
		at smallRoom2 $ sum $ zipWith3 (f delEvt thr x) (zip3 times ids repeatFades) (zip5 mixCoeffs preFx preCoeffs postFx postCoeffs) $ zip3 delRefs refs instrs) throughDel sw sil
	where
		(tapControl, fadeControl, delControl, throughControl) = getControls (loopControl spec)

		dt = 60 / dtBpm 

		times = take len $ times' ++ repeat 1

		postFx = take len $ loopPostfx spec ++ repeat return
		preFx = take len $ loopPrefx spec ++ repeat return
		repeatFades = loopRepeatFades spec ++ repeat 1

		len = length ids
		initMixVals = take len $ loopMixVal spec ++ repeat 0.5
		initPreVals = take len $ loopPrefxVal spec ++ repeat 0.5
		initPostVals = take len $ loopPostfxVal spec ++ repeat 0.5

		silencer 
			| null (loopFades spec) = fmap return ids
			| otherwise               = loopFades spec

		initInstr = loopInitInstr spec

		ids = [0 .. length instrs - 1]
		through = throughControl
		delete = delControl

		throughDel = hlift2' 6 1 (\a b -> (a, b)) through delete
		sw = tapControl (fmap show ids) initInstr		 
		sil = hlifts id $ zipWith (\f n -> f (show n)) fadeControl [0 .. length silencer - 1]

		maxDel = 3

		f :: Tick -> Sig -> Sig -> (D, Int, Sig) -> (Sig, FxFun, Sig, FxFun, Sig) -> (Ref Sig, Ref Sig, a) -> SE Sig2
		f delEvt thr x (t, n, repeatFadeWeight) (mixCoeff, preFx, preCoeff, postFx, postCoeff) (delRef, silRef, instr) = do
			silVal <- readRef silRef	
			runEvt delEvt $ \_ -> do
				a <- readRef delRef
				when1 isCurrent $ writeRef delRef (ifB (a + 1 `lessThan` maxDel) (a + 1) 0)
			delVal <- readRef delRef
			echoSig <- playSf 0

			let d0 = delVal ==* 0
			    d1 = delVal ==* 1
			    d2 = delVal ==* 2

			let playEcho dId = mul (smooth 0.05 $ ifB dId 1 0) $ mul (smooth 0.1 silVal) $ at (echo (dt * t) (ifB dId repeatFadeWeight 0)) $ ifB dId echoSig 0

			mul mixCoeff $ mixAt postCoeff postFx $ sum [ sum $ fmap playEcho [d0, d1, d2]
				, playSf 1]
			where 
				playSf thrVal = mixAt preCoeff preFx $ playInstr (isCurrent &&* thr ==* thrVal) instr
				isCurrent = x ==* (sig $ int n)

		setSilencer refs silIds evt = runEvt evt $ \v -> 
			mapM_ (\ref -> writeRef ref $ sig v) $ fmap (refs !! ) silIds

tabSigs :: Output Sig -> Input Sig -> Sig -> [Sig] -> SE [Sig]
tabSigs writeWidget readWidget switch initVals = do	
	refs <- mapM newGlobalRef initVals	

	vs <- mapM readRef refs
	runEvt (changedE [switch]) $ \_ -> do
		mapM_  (\(v, x) -> when1 (x ==* switch) $ writeWidget v) $ zip vs $ fmap (sig . int) [0 .. length initVals - 1]

	forM_ (zip [0..] refs) $ \(n, ref) -> do
		when1 ((sig $ int n) ==* switch) $ writeRef ref readWidget

	return vs
