{-# Language GADTs #-}
module Csound.Render.Mix(
    Sco, Mix, sco, mix, midi, pgmidi,
    renderCsd, renderCsdBy, writeCsd, writeCsdBy
) where

import Control.Monad(zipWithM_)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Data.List(transpose)
import Data.Monoid
import Data.Maybe(catMaybes)
import Data.Tuple(swap)
import Data.Foldable hiding (mapM_, sum)
import Data.Traversable hiding (mapM)
import Data.Default

import qualified Data.IntMap as IM
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

outArity :: Out a => a -> Int
outArity a = arityCsdTuple (proxy a)
    where proxy :: Out a => a -> NoSE a
          proxy = undefined  

data Arity = Arity
    { arityIns  :: Int
    , arityOuts :: Int }

type InstrId = Int

type InstrTab a = IM.IntMap a
type PreSndTab = DM.IndexMap SndSrc

data SndSrc = SndSrc Arity (SE [Sig]) | MidiSndSrc Arity (SE [Sig]) MidiType Channel
data Mixing = Mixing Arity ([Sig] -> SE [Sig]) (Sco MixNote)

data MixE = MixE
    { mixExpE :: E
    , mixExpSco :: Sco MixNote }

type Sco a = Track Double a

data Mix a where
    Sco :: Arity -> SE [Sig] -> Sco Note -> Mix a
    Mix :: Arity -> ([Sig] -> SE [Sig]) -> Sco (Mix a) -> Mix b
    Mid :: Arity -> SE [Sig] -> MidiType -> Channel -> Mix a


data MixNote = MixNote InstrId | SndNote InstrId (Sco Note) | MidNote MidiInstrParams
  
tempAs :: Sco b -> a -> Sco a
tempAs a = stretch (dur a) . temp

sco :: (Arg a, Out b) => (a -> b) -> Sco a -> Sco (Mix (NoSE b))
sco instr notes = tempAs notes $ Sco (getArity instr) (toOut $ instr toArg) $ fmap (toNote argMethods) notes
    where getArity :: (Arg a, Out b) => (a -> b) -> Arity
          getArity f = let (a, b) = funProxy f in Arity (arity argMethods a) (outArity b)           

mix :: (Out a, Out b) => (a -> b) -> Sco (Mix a) -> Sco (Mix (NoSE b))
mix effect sigs = tempAs sigs $ Mix (getArity effect) (toOut . effect . fromOut) sigs
    where getArity :: (Out a, Out b) => (a -> b) -> Arity
          getArity f = let (a, b) = funProxy f in Arity (outArity a) (outArity b)

midi :: (Out a) => Channel -> (Msg -> a) -> Sco (Mix (NoSE a))
midi chn f = temp $ Mid (getMidiArity f) (toOut $ f Msg) Massign chn

pgmidi :: (Out a) => Maybe Int -> Channel -> (Msg -> a) -> Sco (Mix (NoSE a))
pgmidi mchn n f = temp $ Mid (getMidiArity f) (toOut $ f Msg) (Pgmassign mchn) n

getMidiArity :: (Out a) => (Msg -> a) -> Arity
getMidiArity f = Arity 0 $ outArity $ snd $ funProxy f

funProxy :: (a -> b) -> (a, b)
funProxy = const (undefined, undefined)  

clipByMax :: Out a => Sco (Mix a) -> Sco (Mix a)
clipByMax a = tempAs a $ Mix (getArity a) (return . fmap clip') a
    where clip' x = clip x 0 zeroDbfs

          getArity :: Out a => Sco (Mix a) -> Arity
          getArity a = let v = outArity (proxy a) in Arity v v

          proxy :: Sco (Mix a) -> a
          proxy = undefined  

rescale :: Sco (Mix a) -> Sco (Mix a)
rescale = tmap $ \e -> let factor = (eventDur e / (mixDur $ eventContent e))
                       in  mixStretch factor (eventContent e)
    where mixDur :: Mix a -> Double
          mixDur x = case x of
            Sco _ _ a -> dur a
            Mix _ _ a -> dur a
            Mid _ _ _ _ -> 1

          mixStretch :: Double -> Mix a -> Mix a
          mixStretch k x = case x of
            Sco ar a sco -> Sco ar a $ stretch k sco
            Mix ar a sco -> Mix ar a $ stretch k sco
            Mid _ _ _ _  -> x     


renderCsd :: (Out a) => Sco (Mix a) -> IO String
renderCsd = renderCsdBy def

renderCsdBy :: (Out a) => CsdOptions -> Sco (Mix a) -> IO String
renderCsdBy opt as = render opt $ rescale $ clipByMax as

writeCsd :: (Out a) => String -> Sco (Mix a) -> IO ()
writeCsd file sco = writeFile file =<< renderCsd sco 

writeCsdBy :: (Out a) => CsdOptions -> String -> Sco (Mix a) -> IO ()
writeCsdBy opt file sco = writeFile file =<< renderCsdBy opt sco

render :: (Out a) => CsdOptions -> Sco (Mix a) -> IO String
render opt a = do
    snds <- getSoundSources a
    (lastInstrId, preMixTab) <- getMixing snds a    
    let mixTab = fmap (defMixTab . mixExp) preMixTab
        midiParams = getMidiInstrParams snds
        midiInstrs = fmap (\(MidiInstrParams _ instrId ty chn) -> MidiAssign ty chn instrId) midiParams
        resetMidiInstrId = succ lastInstrId
        sndTab = IM.mapWithKey (\key -> defTab . sndExp key) $ tableSoundSources snds
        notes = getNotes mixTab
        strs = stringMap notes
        ftables = tabMap (IM.elems sndTab ++ (fmap mixExpE $ IM.elems mixTab)) notes
        midiResetInstrNote = if null midiParams then empty else alwayson resetMidiInstrId
    return $ show $ ppCsdFile 
        -- flags
        (renderFlags opt)
        -- instr 0 
        (renderInstr0 (nchnls a) midiInstrs opt $$ portUpdateStmt $$ midiInits midiParams) 
        -- orchestra
        (renderSnd krateSet (fmap (substInstrTabs ftables) sndTab)
            $$ renderMix krateSet (fmap (substMixFtables strs ftables) mixTab) 
            $$ midiReset resetMidiInstrId midiParams)           
        -- scores
        (alwayson lastInstrId $$ midiResetInstrNote)
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

          alwayson instrId = ppNote instrId 0 (dur a) []  

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
            MidiSndSrc arity _ ty chn -> Just $ MidiInstrParams arity n ty chn
            _ -> Nothing

initMidiVar :: Var -> Doc
initMidiVar a = ppOpc (ppVar a) "init" [int 0]

resetMidiVarInstr :: [Var] -> E
resetMidiVarInstr vs = execSE $ mapM_ (flip writeVar (0 :: Sig)) vs

midiVar :: InstrId -> Int -> Var
midiVar instrId portId = Var GlobalVar Ar ("midi_" ++ show instrId ++ "_" ++ show portId)
 
getNotes :: InstrTab MixE -> Note
getNotes = foldMap (foldMap scoNotes . mixExpSco)
    where scoNotes :: MixNote -> Note
          scoNotes x = case x of
            SndNote n sco -> fold sco
            _ -> mempty    

sndExp :: InstrId -> SndSrc -> E
sndExp instrId x = execSE $ case x of
    SndSrc arity sigs -> outs arity =<< sigs
    MidiSndSrc arity sigs mType chn -> midiOuts instrId =<< sigs

mixExp :: Mixing -> MixE
mixExp (Mixing arity effect sco) = MixE exp sco
    where exp = execSE $ outs arity . mixMidis midiNotes =<< effect =<< ins arity
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


midiOuts :: InstrId -> [Sig] -> SE ()
midiOuts instrId as = zipWithM_ (\portId sig -> writeVar (midiVar instrId portId) sig) [1 .. ] as

outs :: Arity -> [Sig] -> SE ()
outs arity sigs = zipWithM_ (out arity) [1 .. arityOuts arity] sigs

ins  :: Arity -> SE [Sig]
ins  arity = mapM in_ [1 .. arityIns arity] 

out :: Arity -> Int -> Sig -> SE ()
out arity n sig = chnmix sig $ portName n (p $ succ $ arityIns arity) 

in_ :: Int -> SE Sig
in_ n = chnget (portName n $ readVar portVar)

portFormatString :: Int -> Str
portFormatString n = str $ show n ++ "_" ++ "%d"

portName :: Int -> D -> Str
portName n = sprintf (portFormatString n) . return

chnmix :: Sig -> Str -> SE ()
chnmix a b = se_ $ opc2 "chnmix" [(Xr, [Ar, Sr])] a b

chnclear :: Str -> SE ()
chnclear a = se_ $ opc1 "chnclear" [(Xr, [Sr])] a

chnget :: Str -> SE Sig
chnget a = se $ opc1 "chnget" [(Ar, [Sr])] a


renderSnd :: KrateSet -> IM.IntMap E -> Doc
renderSnd krateSet = ppOrc . fmap (uncurry $ renderInstr krateSet) . IM.toList
 
renderMix :: KrateSet -> IM.IntMap MixE -> Doc
renderMix krateSet = ppOrc . fmap (uncurry render) . IM.toList
    where render instrId (MixE exp sco) = ppInstr instrId $ (renderPort $$ renderSco sco) : renderInstrBody krateSet exp
          renderPort = ppOpc (ppVar portVar) "FreePort" []
          renderSco a = ppSco $ renderNote =<< T.render a
          renderNote e = case eventContent e of
              MixNote n     -> return $ ppEvent n (eventStart e) (eventDur e) [] portVar
              SndNote n sco -> fmap (\x -> ppEvent n (eventStart x) (eventDur x) (eventContent x) portVar) $ T.render $ delay (eventStart e) sco -- only delay, stretch was done before
              MidNote _ -> mempty  
          
portVar :: Var
portVar = Var LocalVar Ir "Port"

tableSoundSources :: PreSndTab -> InstrTab SndSrc
tableSoundSources = IM.fromList . fmap swap . DM.elems

getSoundSources :: Sco (Mix a) -> IO PreSndTab
getSoundSources = flip execState (return DM.empty) . getSndSrcSco

type MkIndexMap = State (IO PreSndTab) ()

getSndSrcSco :: Sco (Mix a) -> MkIndexMap
getSndSrcSco sco = traverse getSndSrcMix sco >> return ()
    
getSndSrcMix :: Mix a -> MkIndexMap
getSndSrcMix x = case x of
    Mix ar eff sco    -> getSndSrcSco sco
    Sco ar snd sco    -> saveSndSrc $ SndSrc ar snd
    Mid ar snd ty chn -> saveSndSrc $ MidiSndSrc ar snd ty chn
    
saveSndSrc :: SndSrc -> MkIndexMap
saveSndSrc a = modify (DM.insert a =<<)

-- hard stuff

type MkMixing a = StateT MixingState IO a

data MixingState = MixingState 
    { counter :: Int
    , elems   :: [(Int, Mixing)] }


initMixingState :: Int -> MixingState
initMixingState n = MixingState n []

saveElem :: Int -> Mixing -> MkMixing ()
saveElem n a = modify $ \x -> x{ elems = (n, a) : elems x }

getCounter :: MkMixing Int
getCounter = fmap counter get

putCounter :: Int -> MkMixing ()
putCounter n = modify $ \s -> s{ counter = n }

getMixing :: PreSndTab -> Sco (Mix a) -> IO (InstrId, InstrTab Mixing)
getMixing tab sco = fmap formRes $ 
    execStateT (traverse (getMixingMix tab) sco) 
               (initMixingState $ DM.length tab + numOfInstrSco sco)
    where formRes x = (fst $ last $ elems x, IM.fromList $ elems x)

getMixingMix :: PreSndTab -> Mix a -> MkMixing MixNote
getMixingMix tab x = case x of
    Sco ar snd sco -> do
        Just n <- lift $ DM.lookup (SndSrc ar snd) tab
        return $ SndNote n sco
    Mid ar snd ty chn -> do
        Just n <- lift $ DM.lookup (MidiSndSrc ar snd ty chn) tab
        return $ MidNote (MidiInstrParams ar n ty chn)
    Mix ar eff sco -> do
        n <- getCounter
        putCounter $ pred n
        notes <- traverse (getMixingMix tab) sco
        saveElem n $ Mixing ar eff notes
        return $ MixNote n
        
numOfInstrSco :: Sco (Mix a) -> Int
numOfInstrSco as = getSum $ foldMap (Sum . numOfInstrForMix) as

numOfInstrForMix :: Mix a -> Int
numOfInstrForMix x = case x of
    Mix _ _ a -> 1 + numOfInstrSco a
    Sco _ _ _ -> 0
    Mid _ _ _ _ -> 0

portUpdateStmt = verbatimLines [
    "giPort init 0",
    "opcode FreePort, i, 0",
    "xout giPort",
    "giPort = giPort + 1",
    "endop"]


