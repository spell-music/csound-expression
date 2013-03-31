{-# Language GADTs, DeriveFunctor, DeriveFoldable #-}
module Csound.Render.Mix(
    Sco, Mix, sco, mix, midi, pgmidi,
    renderCsd, renderCsdBy, writeCsd, writeCsdBy, playCsd, playCsdBy, 
    effect, effectS
) where

import Control.Monad(zipWithM_)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Arrow(second)

import Data.List(transpose)
import Data.Monoid
import Data.Maybe(catMaybes)
import Data.Tuple(swap)
import Data.Foldable hiding (mapM_, sum)
import Data.Traversable hiding (mapM)
import Data.Default

import System.Cmd(system)

import qualified Data.Set    as S

import qualified Csound.Render.IndexMap as DM

import Temporal.Media(temp, stretch, dur, Track, Event(..), tmap, delay)
import qualified Temporal.Media as T

import Csound.Exp hiding (Event(..))
import Csound.Exp.Numeric
import Csound.Exp.Wrapper
import Csound.Exp.Cons
import Csound.Render.Pretty
import Csound.Render.Instr
import Csound.Tfm.RateGraph(KrateSet)
import Csound.Render.Options
import Csound.Render.Sco(stringMap, substNoteStrs, StringMap, MidiType(..))

import Csound.Opcode(clip, zeroDbfs, sprintf)

un = undefined

-- | Renders Csound file.
renderCsd :: (Out a) => Sco (Mix a) -> IO String
renderCsd = renderCsdBy def

-- | Renders Csound file with options.
renderCsdBy :: (Out a) => CsdOptions -> Sco (Mix a) -> IO String
renderCsdBy opt as = render opt $ rescale as

-- | Render Csound file and save it to the give file.
writeCsd :: (Out a) => String -> Sco (Mix a) -> IO ()
writeCsd file sco = writeFile file =<< renderCsd sco 

-- | Render Csound file with options and save it to the give file.
writeCsdBy :: (Out a) => CsdOptions -> String -> Sco (Mix a) -> IO ()
writeCsdBy opt file sco = writeFile file =<< renderCsdBy opt sco

-- | RenderCsound file save it to the given file, render with csound command and play it with the given program.
-- 
-- > playCsd program file sco 
--
-- Produces files @file.csd@ (with 'Csound.Render.Mix.renderCsd') and @file.wav@ (with @csound@) and then invokes:
--
-- > program file.wav
playCsd :: (Out a) => String -> String -> Sco (Mix a) -> IO ()
playCsd = playCsdBy def

-- | Works just like 'Csound.Render.Mix.playCsd' but you can supply csound options.
playCsdBy :: (Out a) => CsdOptions -> String -> String -> Sco (Mix a) -> IO ()
playCsdBy opt player file sco = do
    writeCsdBy opt fileCsd sco
    system $ "csound -o " ++ fileWav ++ " " ++ fileCsd
    system $ player ++ " " ++ fileWav
    return ()
    where fileCsd = file ++ ".csd"
          fileWav = file ++ ".wav"  

-- | A bunch of events.
type Sco a = Track Double a

-- | Track of sound. 
data Mix a where
    Sco :: Instr -> Sco Note -> Mix a
    Mid :: Instr -> MidiType -> Channel -> Mix a
    Mix :: Arity -> ([Sig] -> SE [Sig]) -> Sco (Mix a) -> Mix b

-- | Play a bunch of notes with the given instrument.
--
-- > res = sco instrument scores 
--
-- * @instrument@ is a function that takes notes and produces a tuple of signals (maybe with some side effect)
--  
-- * @scores@ are some notes (see the module "Temporal.Media" on how to build complex scores out of simple ones)
--
-- Let's try to understand the type of the output. It's @Sco (Mix (NoSE a))@. What does it mean? Let's look at the different parts of this type:
--
-- * @Sco a@ - you can think of it as a container of some values of type @a@ (every value of type @a@ starts at some time and lasts for some time in seconds)
--
-- * @Mix a@ - is an output of Csound instrument it can be one or several signals ('Csound.Base.Sig' or 'Csound.Base.CsdTuple'). 
--
-- *NoSE a* - it's a tricky part of the output. 'NoSE' means literaly 'no SE'. It tells to the type checker that it can skip the 'Csound.Base.SE' wrapper
-- from the type 'a' so that @SE a@ becomes just @a@ or @SE (a, SE b, c)@ becomes @(a, b, c)@. Why should it be? I need 'SE' to deduce the order of the
-- instruments that have side effects. I need it within one instrument. But when instrument is rendered i no longer need 'SE' type. So 'NoSE' lets me drop it
-- from the output type. 
sco :: (Arg a, Out b) => (a -> b) -> Sco a -> Sco (Mix (NoSE b))
sco instr notes = tempAs notes $ Sco (Instr (DM.makeInstrName instr) (getArity instr) (toOut $ instr toArg)) $ fmap (toNote argMethods) notes
    where getArity :: (Arg a, Out b) => (a -> b) -> Arity
          getArity f = let (a, b) = funProxy f in Arity (arity argMethods a) (outArity b)           

-- | Applies an effect to the sound. Effect is applied to the sound on the give track. 
--
-- > res = mix effect sco 
--
-- * @effect@ - a function that takes a tuple of signals and produces a tuple of signals.
--
-- * @sco@ - something that is constructed with 'Csound.Base.sco' or 'Csound.Base.mix' or 'Csound.Base.midi'. 
--
-- With the function 'Csound.Base.mix' you can apply a reverb or adjust the level of the signal. It functions like a mixing board
-- but unlike mixing board it produces the value that you can arrange with functions from the module "Temporal.Media". You can delay it
-- mix with some other track and apply some another effect on top of it!
mix :: (Out a, Out b) => (a -> b) -> Sco (Mix a) -> Sco (Mix (NoSE b))
mix effect sigs = tempAs sigs $ Mix (getArity effect) (toOut . effect . fromOut) sigs
    where getArity :: (Out a, Out b) => (a -> b) -> Arity
          getArity f = let (a, b) = funProxy f in Arity (outArity a) (outArity b)

-- | Triggers a midi-instrument (like Csound's massign). The result type is a fake one. It's wrapped in the 'Csound.Base.Sco' for the ease of mixing.
-- you can not delay or stretch it. The only operation that is meaningful for it is 'Temporal.Media.chord'. But you can add effects to it with 'Csound.Base.mix'!
midi :: (Out a) => Channel -> (Msg -> a) -> Sco (Mix (NoSE a))
midi chn f = temp $ Mid (Instr (DM.makeInstrName f) (getMidiArity f) (toOut $ f Msg)) Massign chn

-- | Triggers a - midi-instrument (like Csound's pgmassign). 
pgmidi :: (Out a) => Maybe Int -> Channel -> (Msg -> a) -> Sco (Mix (NoSE a))
pgmidi mchn n f = temp $ Mid (Instr (DM.makeInstrName f) (getMidiArity f) (toOut $ f Msg)) (Pgmassign mchn) n


-- | Constructs the effect that applies a given function on every channel.
effect :: (CsdTuple a, Out a) => (Sig -> Sig) -> (a -> a)
effect f = toCsdTuple . fmap (toE . f . fromE) . fromCsdTuple

-- | Constructs the effect that applies a given function with side effect (it uses random opcodes or delays) on every channel.
effectS :: (CsdTuple a, Out a) => (Sig -> SE Sig) -> (a -> SE a)
effectS f a = fmap fromOut $ mapM f =<< toOut a

outArity :: Out a => a -> Int
outArity a = arityCsdTuple (proxy a)
    where proxy :: Out a => a -> NoSE a
          proxy = undefined  

data Arity = Arity
    { arityIns  :: Int
    , arityOuts :: Int }

type InstrId = Int

data MixInstrTab a = MixInstrTab 
    { masterInstr :: (InstrId, a)
    , otherInstr  :: [(InstrId, a)] 
    } deriving (Functor)

instance Foldable MixInstrTab where
    foldMap f = foldMap f . mixInstrTabElems

mixInstrTabElems :: MixInstrTab a -> [a]
mixInstrTabElems (MixInstrTab master other) = snd master : fmap snd other

newtype InstrTab a = InstrTab { unInstrTab :: [(InstrId, a)] }
    deriving (Functor, Foldable) 
    
mapWithKey :: (InstrId -> a -> b) -> InstrTab a -> InstrTab b   
mapWithKey f (InstrTab as) = InstrTab $ fmap (\(n, a) -> (n, f n a)) as

elems :: InstrTab a -> [a]
elems (InstrTab as) = fmap snd as
    
type PreSndTab = DM.IndexMap SndSrc

data Instr = Instr DM.InstrName Arity (SE [Sig])

data SndSrc
    = SndSrc Instr     
    | MidiSndSrc Instr MidiType Channel

data Mixing = Mixing Arity ([Sig] -> SE [Sig]) (Sco MixNote)

data MixE = MixE
    { mixExpE :: E
    , mixExpSco :: Sco MixNote }

data MixNote = MixNote InstrId | SndNote InstrId (Sco Note) | MidNote MidiInstrParams
  
tempAs :: Sco b -> a -> Sco a
tempAs a = stretch (dur a) . temp

getMidiArity :: (Out a) => (Msg -> a) -> Arity
getMidiArity f = Arity 0 $ outArity $ snd $ funProxy f

funProxy :: (a -> b) -> (a, b)
funProxy = const (undefined, undefined)  

clipByMax :: [Sig] -> SE [Sig]
clipByMax = return . fmap clip'
    where clip' x = clip x 0 zeroDbfs

rescale :: Sco (Mix a) -> Sco (Mix a)
rescale = tmap $ \e -> let factor = (eventDur e / (mixDur $ eventContent e))
                       in  mixStretch factor (eventContent e)
    where mixDur :: Mix a -> Double
          mixDur x = case x of
            Sco _ a -> dur a
            Mix _ _ a -> dur a
            Mid _ _ _ -> 1

          mixStretch :: Double -> Mix a -> Mix a
          mixStretch k x = case x of
            Sco a sco -> Sco a $ stretch k sco
            Mix ar a sco -> Mix ar a $ rescale $ stretch k sco
            Mid _ _ _  -> x     

getLastInstrId :: MixInstrTab a -> Int
getLastInstrId = fst . masterInstr

render :: (Out a) => CsdOptions -> Sco (Mix a) -> IO String
render opt a = do
    snds <- getSoundSources a
    preMixTab <- getMixing snds a    
    let lastInstrId = getLastInstrId preMixTab
        mixTab = fmap defMixTab $ mixExps preMixTab
        midiParams = getMidiInstrParams snds
        midiInstrs = fmap (\(MidiInstrParams _ instrId ty chn) -> MidiAssign ty chn instrId) midiParams
        resetMidiInstrId = succ lastInstrId
        sndTab = mapWithKey (\key -> defTab . sndExp key) $ tableSoundSources snds
        notes = getNotes mixTab
        strs = stringMap notes
        ftables = tabMap (elems sndTab ++ (fmap mixExpE $ mixInstrTabElems mixTab)) notes
        midiResetInstrNote = if null midiParams then empty else alwayson totalDur resetMidiInstrId        
        mixTabSubstituted = fmap (substMixFtables strs ftables) mixTab
    return $ show $ ppCsdFile 
        -- flags
        (renderFlags opt)
        -- instr 0 
        (renderInstr0 (nchnls a) midiInstrs opt $$ portUpdateStmt $$ midiInits midiParams) 
        -- orchestra
        (renderSnd krateSet (fmap (substInstrTabs ftables) sndTab)
            $$ renderMix krateSet mixTabSubstituted
            $$ midiReset resetMidiInstrId midiParams)           
        -- scores
        (lastInstrNotes totalDur (masterInstr mixTabSubstituted) $$ midiResetInstrNote)
        -- strings
        (ppMapTable ppStrset strs)
        -- ftables
        (ppTotalDur (dur a) $$ ppMapTable ppTabDef ftables)
    where substMixFtables :: StringMap -> TabMap -> MixE -> MixE
          substMixFtables strMap tabMap (MixE exp sco) = MixE (substInstrTabs tabMap exp) (fmap substNote sco)
              where substNote x = case x of
                        SndNote n sco -> SndNote n $ fmap (substNoteStrs strMap . substNoteTabs tabMap) sco
                        _ -> x

          krateSet = S.fromList $ csdKrate opt        

          defTab :: E -> E
          defTab = defineInstrTabs (tabResolution opt)

          defMixTab :: MixE -> MixE
          defMixTab (MixE eff sco) = MixE (defTab eff) (fmap defNoteTab sco) 
              where defNoteTab x = case x of
                        SndNote n sco -> SndNote n $ fmap (defineNoteTabs $ tabResolution opt) sco      
                        _ -> x
                        
          totalDur = dur a
          
alwayson totalDur instrId = ppNote instrId 0 totalDur []      

nchnls :: Out a => Sco (Mix a) -> Int
nchnls = outArity . proxy  
    where proxy :: Sco (Mix a) -> a
          proxy = undefined  
          
data MidiInstrParams = MidiInstrParams Arity InstrId MidiType Channel

midiInits :: [MidiInstrParams] -> Doc
midiInits = vcat . fmap initMidiVar . (getMidiVars =<< )

midiReset :: InstrId -> [MidiInstrParams] -> Doc
midiReset n = ppInstr n . fmap reset . (getMidiVars =<< ) 
    where reset v = ppVar v $= int 0

getMidiVars :: MidiInstrParams -> [Var]
getMidiVars (MidiInstrParams arity instrId _ _) = fmap (midiVar instrId) [1 .. arityOuts arity]

getMidiInstrParams :: PreSndTab -> [MidiInstrParams]
getMidiInstrParams a = catMaybes $ fmap extract $ DM.elems a
    where extract (x, n) = case x of
            MidiSndSrc (Instr _ arity _) ty chn -> Just $ MidiInstrParams arity n ty chn
            _ -> Nothing

initMidiVar :: Var -> Doc
initMidiVar a = ppOpc (ppVar a) "init" [int 0]

resetMidiVarInstr :: [Var] -> E
resetMidiVarInstr vs = execSE $ mapM_ (flip writeVar (0 :: Sig)) vs

midiVar :: InstrId -> Int -> Var
midiVar instrId portId = Var GlobalVar Ar ("midi_" ++ show instrId ++ "_" ++ show portId)
 
getNotes :: MixInstrTab MixE -> Note
getNotes = foldMap (foldMap scoNotes . mixExpSco)
    where scoNotes :: MixNote -> Note
          scoNotes x = case x of
            SndNote n sco -> fold sco
            _ -> mempty    

sndExp :: InstrId -> SndSrc -> E
sndExp instrId x = execSE $ case x of
    SndSrc (Instr _ arity sigs) -> outs (4 + arityIns arity) =<< sigs   -- 4 + arity because there are 3 first arguments (instrId, start, dur) and arity params comes next
    MidiSndSrc (Instr _ arity sigs) mType chn -> midiOuts instrId =<< sigs



mixExps :: MixInstrTab Mixing -> MixInstrTab MixE
mixExps (MixInstrTab master other) = MixInstrTab (second masterMixExp master) (fmap (second mixExp) other) 

masterMixExp    = mixExpGen masterOuts
mixExp          = mixExpGen (outs 4) -- for mixing instruments we expect the port number to be the fourth parameter

mixExpGen :: ([Sig] -> SE ()) -> Mixing -> MixE
mixExpGen formOuts (Mixing arity effect sco) = MixE exp sco
    where exp = execSE $ formOuts . mixMidis midiNotes =<< effect =<< ins arity
          midiNotes = foldMap getMidiFromMixNote sco

mixMidis :: [MidiInstrParams] -> [Sig] -> [Sig]
mixMidis ms sigs 
    | null ms   = sigs
    | otherwise = zipWith (+) midiSums sigs
    where midiSums = fmap sum $ transpose $ fmap (fmap readVar . getMidiVars) ms

getMidiFromMixNote :: MixNote -> [MidiInstrParams] 
getMidiFromMixNote x = case x of
    MidNote a -> [a]
    _ -> []

masterOuts :: [Sig] -> SE ()
masterOuts xs = se_ $ case xs of
    a:[] -> opc1 "out" [(Xr, [Ar])] a
    _    -> opcs "outs" [(Xr, repeat Ar)] xs    

midiOuts :: InstrId -> [Sig] -> SE ()
midiOuts instrId as = zipWithM_ (\portId sig -> writeVar (midiVar instrId portId) sig) [1 .. ] as

outs :: Int -> [Sig] -> SE ()
outs readPortId sigs = zipWithM_ (out readPortId) [1 .. ] sigs

ins  :: Arity -> SE [Sig]
ins  arity = mapM in_ [1 .. arityIns arity] 

out :: Int -> Int -> Sig -> SE ()
out readPortId n sig = chnmix sig $ portName n (p readPortId) 

in_ :: Int -> SE Sig
in_ n = do
    sig <- chnget name
    chnclear name
    return sig    
    where name = portName n $ readVar portVar

portFormatString :: Int -> Str
portFormatString n = str $ 'p' : show n ++ "_" ++ "%d"

portName :: Int -> D -> Str
portName n = sprintf (portFormatString n) . return

chnmix :: Sig -> Str -> SE ()
chnmix a b = se_ $ opc2 "chnmix" [(Xr, [Ar, Sr])] a b

chnclear :: Str -> SE ()
chnclear a = se_ $ opc1 "chnclear" [(Xr, [Sr])] a

chnget :: Str -> SE Sig
chnget a = se $ opc1 "chnget" [(Ar, [Sr])] a


renderSnd :: KrateSet -> InstrTab E -> Doc
renderSnd krateSet = ppOrc . fmap (uncurry $ renderInstr krateSet) . unInstrTab
 
renderMix :: KrateSet -> MixInstrTab MixE -> Doc
renderMix krateSet (MixInstrTab master other) = (ppOrc . (uncurry renderMaster master : ) . fmap (uncurry render)) other
    where renderMaster instrId (MixE exp _) = ppInstr instrId $ renderMasterPort : renderInstrBody krateSet exp
          render instrId (MixE exp sco) = ppInstr instrId $ (renderPort $$ renderSco ppEvent sco) : renderInstrBody krateSet exp          
          renderPort = ppOpc (ppVar portVar) "FreePort" []           
          renderMasterPort = ppVar portVar $= int 0

renderSco :: (InstrId -> Event Double [Prim] -> Var -> Doc) -> Sco MixNote -> Doc
renderSco formNote a = ppSco $ renderNote =<< T.render a
    where renderNote e = case eventContent e of
              MixNote n     -> return $ formNote n (fmap (const []) e) portVar
              SndNote n sco -> fmap (\x -> formNote n x portVar) $ T.render $ delay (eventStart e) sco -- only delay, stretch was done before
              MidNote _     -> mempty

              
lastInstrNotes :: Double -> (InstrId, MixE) -> Doc
lastInstrNotes totalDur (instrId, a) = alwayson totalDur instrId $$ sco
    where sco = renderSco (\n evt var -> ppMasterNote n evt) $ mixExpSco a
  
       
portVar :: Var
portVar = Var LocalVar Ir "Port"

tableSoundSources :: PreSndTab -> InstrTab SndSrc
tableSoundSources = InstrTab . fmap swap . DM.elems

getSoundSources :: Sco (Mix a) -> IO PreSndTab
getSoundSources = flip execState (return $ DM.empty 1) . getSndSrcSco

type MkIndexMap = State (IO PreSndTab) ()

getSndSrcSco :: Sco (Mix a) -> MkIndexMap
getSndSrcSco sco = traverse getSndSrcMix sco >> return ()
    
getSndSrcMix :: Mix a -> MkIndexMap
getSndSrcMix x = case x of
    Mix ar eff sco   -> getSndSrcSco sco
    Sco instr sco    -> saveSndSrc $ SndSrc instr
    Mid instr ty chn -> saveSndSrc $ MidiSndSrc instr ty chn
    
saveSndSrc :: SndSrc -> MkIndexMap
saveSndSrc a = modify (DM.insert (sndSrcName a) a =<<)

sndSrcName :: SndSrc -> DM.InstrName
sndSrcName = instrName . sndSrcInstr

instrName :: Instr -> DM.InstrName
instrName (Instr name _ _) = name

sndSrcInstr :: SndSrc -> Instr
sndSrcInstr x = case x of
    SndSrc instr -> instr
    MidiSndSrc instr _ _ -> instr

-- hard stuff

type MkMixing a = StateT MixingState IO a

data MixingState = MixingState 
    { counterSt :: Int
    , elemsSt   :: [(Int, Mixing)] }


initMixingState :: Int -> MixingState
initMixingState n = MixingState n []

saveElem :: Int -> Mixing -> MkMixing ()
saveElem n a = modify $ \x -> x{ elemsSt = (n, a) : elemsSt x }

getCounter :: MkMixing Int
getCounter = fmap counterSt get

putCounter :: Int -> MkMixing ()
putCounter n = modify $ \s -> s{ counterSt = n }

getMixing :: Out a => PreSndTab -> Sco (Mix a) -> IO (MixInstrTab Mixing)
getMixing tab sco = fmap formRes $ 
    runStateT (traverse (getMixingMix tab) sco) 
               (initMixingState $ pred lastInstrId)
    where formRes (sco, st) = MixInstrTab (lastInstrId, Mixing (Arity n n) clipByMax sco) (elemsSt st)
          lastInstrId = 1 + DM.length tab + numOfInstrSco sco
          n = nchnls sco     
                             

getMixingMix :: PreSndTab -> Mix a -> MkMixing MixNote
getMixingMix tab x = case x of
    Sco snd sco -> do
        Just n <- lift $ DM.lookup (instrName snd) tab
        return $ SndNote n sco
    Mid snd ty chn -> do
        Just n <- lift $ DM.lookup (instrName snd) tab
        return $ MidNote (MidiInstrParams (instrArity snd) n ty chn)
    Mix ar eff sco -> do
        n <- getCounter
        putCounter $ pred n
        notes <- traverse (getMixingMix tab) sco
        saveElem n $ Mixing ar eff notes
        return $ MixNote n
        
instrArity (Instr _ ar _) = ar

numOfInstrSco :: Sco (Mix a) -> Int
numOfInstrSco as = getSum $ foldMap (Sum . numOfInstrForMix) as

numOfInstrForMix :: Mix a -> Int
numOfInstrForMix x = case x of
    Mix _ _ a -> 1 + numOfInstrSco a
    Mid _ _ _ -> 0
    Sco _ _   -> 0

portUpdateStmt = verbatimLines [
    "giPort init 1",
    "opcode FreePort, i, 0",
    "xout giPort",
    "giPort = giPort + 1",
    "endop"]


