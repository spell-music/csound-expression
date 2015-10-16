
import qualified Sharc.Types as Sh
import Csound.Base
import Csound.Patch
import Sharc.Data
import Sharc.Types

note2sig :: Sh.Note -> Sig
note2sig n = oscBy (harmonics2tab $ Sh.noteHarmonics n) (sig $ double $ Sh.pitchFund $ Sh.notePitch n)

note2tab :: Sh.Note -> Tab
note2tab n = (harmonics2tab $ Sh.noteHarmonics n)

deg x = 180 * x / pi

harmonics2tab harmonics = sines3 $ fmap (\h -> (fromIntegral $ Sh.harmonicId h, Sh.harmonicAmplitude h, deg $ Sh.harmonicPhase h)) harmonics

---------------------------------------------------------------------------
-- oscilliators

sharcOsc :: Sh.Instr -> D -> Sig
sharcOsc instr cpsTab = sigSharcOsc instr cpsTab (sig cpsTab)

sigSharcOsc :: Sh.Instr -> D -> Sig -> Sig
sigSharcOsc = genSharcOsc' oscBy 

rndSharcOsc :: Sh.Instr -> D -> SE Sig
rndSharcOsc instr cpsTab = rndSigSharcOsc instr cpsTab (sig cpsTab)

rndSigSharcOsc :: Sh.Instr -> D -> Sig -> SE Sig
rndSigSharcOsc = genSharcOsc' rndOscBy 

genSharcOsc' :: (Tab -> Sig -> a) -> Sh.Instr -> D -> Sig -> a
genSharcOsc' wave instr cps cpsSig = wave t cpsSig
    where
        t = fromTabListD tabs (cps2pitch cps - int keyMin)        

        tabs = tabList $ fmap (harmonics2tab . Sh.noteHarmonics) ns

        ns = Sh.instrNotes instr
        keys = fmap (Sh.pitchKeyNum . Sh.notePitch) ns
        keyMin = minimum keys        

cps2pitch :: Floating a => a -> a
cps2pitch x =  69 + 12 * logBase 2 (x / 440)

---------------------------------------------------------------------------
-- patches

uni = multiHz 4 (cent 40)

soloSharcOsc :: Sh.Instr -> D -> SE Sig
soloSharcOsc instr cps = mul (fades 0.001 0.05) $ rndSharcOsc instr cps

orcSharcOsc :: Sh.Instr -> D -> SE Sig
orcSharcOsc instr cps = mul (fades 0.01 0.42) $ uni (rndSharcOsc instr . ir) (sig cps)

purePadSharcOsc :: Sh.Instr -> D -> SE Sig
purePadSharcOsc instr cps = mul (fades 0.65 0.75) $ rndSharcOsc instr cps

padSharcOsc :: Sh.Instr -> D -> SE Sig
padSharcOsc instr cps = mul (fades 0.65 0.75) $ uni (rndSharcOsc instr . ir) (sig cps)

-- dreamSharcOsc :: Sh.Instr -> D -> SE Sig

soloSharc :: Sh.Instr -> Patch2
soloSharc instr = Patch 
    { patchInstr = fmap fromMono . onCps (soloSharcOsc instr)
    , patchFx    = fx1 0.25 smallHall2
    }

orcSharc :: Sh.Instr -> Patch2
orcSharc instr = Patch 
    { patchInstr = fmap fromMono . onCps (orcSharcOsc instr)
    , patchFx    = fx1 0.25 largeHall2
    }

padSharc :: Sh.Instr -> Patch2
padSharc instr = Patch 
    { patchInstr = fmap fromMono . onCps (padSharcOsc instr)
    , patchFx    = fx1 0.35 largeHall2
    }

purePadSharc :: Sh.Instr -> Patch2
purePadSharc instr = Patch 
    { patchInstr = fmap fromMono . onCps (purePadSharcOsc instr)
    , patchFx    = fx1 0.35 largeHall2
    }

dreamSharc :: Sh.Instr -> Patch2
dreamSharc instr = dreamPadBy (\cps -> rndSigSharcOsc instr (ir cps) cps)

dreamSharc' :: Sh.Instr -> Sig -> Patch2
dreamSharc' instr brightness = dreamPadBy' brightness (\cps -> rndSigSharcOsc instr (ir cps) cps) 



fx1 :: Sig -> (a -> a) -> [FxSpec a]
fx1 dw f = [FxSpec dw (return . f)]


granTrumpet = dac $ at (grainyDelay 1 0.15 0.75 0.45 35 0.07 1.5) $ atMidi (dreamSharc' trumpetC (0.5 + 0.25 * osc 12) )
