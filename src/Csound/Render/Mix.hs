{-# Language GADTs #-}
module Csound.Render.Mix where

import Control.Monad(zipWithM_)

import Data.Foldable
import Data.Default
import Control.Arrow(second)

import qualified Data.IntMap as IM
import qualified Data.Set    as S

import Temporal.Media(temp, stretch, dur, Track, Event(..), tmap)
import qualified Temporal.Media as T

import Csound.Exp hiding (Event(..))
import Csound.Exp.Wrapper
import Csound.Exp.Cons
import Csound.Render.Pretty(Doc, ($$), ppOrc, ppOpc, ppInstr, ppVar, ppSco, ppEvent)
import Csound.Render.Instr
import Csound.Tfm.RateGraph(KrateSet)
import Csound.Render.Options

import Csound.Opcode(clip, zeroDbfs, sprintf)

un = undefined

data Arity = Arity
    { arityIns  :: Int
    , arityOuts :: Int }

type Instr = E

type Sco a = Track Double a

data Mix a where
    Sco :: SE [Sig] -> Sco Note -> Mix a
    Mix :: ([Sig] -> SE [Sig]) -> Sco (Mix a) -> Mix b

    
tempAs :: Sco b -> a -> Sco a
tempAs a = stretch (dur a) . temp

sco :: (Arg a, Out b) => (a -> b) -> Sco a       -> Sco (Mix (NoSE b))
sco instr notes = tempAs notes $ Sco (toOut $ instr toArg) $ fmap (toNote argMethods) notes

mix :: (Out a, Out b) => (a -> b) -> Sco (Mix a) -> Sco (Mix (NoSE b))
mix effect sigs = tempAs sigs $ Mix (toOut . effect . fromOut) sigs


type InstrId = Int

type InstrTab a = IM.IntMap a

data SndSrc = SndSrc Arity (SE [Sig])
data Mixing = Mixing Arity ([Sig] -> SE [Sig]) (Sco (InstrId, Note))

clipByMax :: Out a => Sco (Mix a) -> Sco (Mix a)
clipByMax a = tempAs a $ Mix (return . fmap clip') a
    where clip' x = clip x 0 zeroDbfs

rescale :: Sco (Mix a) -> Sco (Mix a)
rescale = tmap $ \e -> let factor = (eventDur e / (mixDur $ eventContent e))
                       in  mixStretch factor (eventContent e)
    where mixDur :: Mix a -> Double
          mixDur x = case x of
            Sco _ a -> dur a
            Mix _ a -> dur a

          mixStretch :: Double -> Mix a -> Mix a
          mixStretch k x = case x of
            Sco a sco -> Sco a $ stretch k sco
            Mix a sco -> Mix a $ stretch k sco


renderCsd :: (Out a) => Sco (Mix a) -> IO String
renderCsd as = render def $ rescale $ clipByMax as

render :: (Out a) => CsdOptions -> Sco (Mix a) -> IO String
render opt a = do
    snds <- getSoundSources a
    mixTab <- fmap (fmap (defMixTab . mixExp)) $ getMixing snds a
    let sndTab = fmap (defTab . sndExp) $ tableSoundSources snds
        ftables = tabMap (IM.elems sndTab ++ (fmap mixExpE $ IM.elems mixTab)) (getNotes mixTab)
    return $ show $ renderSnd krateSet (fmap (substInstrTabs ftables) sndTab) $$
             renderMix krateSet (fmap (substMixFtables ftables) mixTab)
    where substMixFtables :: TabMap -> MixE -> MixE
          substMixFtables m (MixE exp sco) = MixE (substInstrTabs m exp) (fmap (second $ substNoteTabs m) sco)

          krateSet = S.fromList $ csdKrate opt        

          defTab :: E -> E
          defTab = defineInstrTabs (tabResolution opt)

          defMixTab :: MixE -> MixE
          defMixTab (MixE eff sco) = MixE (defTab eff) (fmap (second $ defineNoteTabs $ tabResolution opt) sco) 
      

type PreSndTab = IM.IntMap [(SndSrc, Int)]

data MixE = MixE
    { mixExpE :: E
    , mixExpSco :: (Sco (InstrId, Note)) }
    
getNotes :: InstrTab MixE -> [Note]
getNotes = foldMap (scoNotes . mixExpSco)
    where scoNotes = foldMap (return . snd)

sndExp :: SndSrc -> E
sndExp (SndSrc arity sigs) = execSE $ outs arity =<< sigs

mixExp :: Mixing -> MixE
mixExp (Mixing arity effect sco) = MixE exp sco
    where exp = execSE $ outs arity =<< effect =<< ins arity
    
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
          renderSco a = ppSco $ fmap (\e -> ppEvent (fst $ eventContent e) (eventStart e) (eventDur e) (snd $ eventContent e) portVar) $ T.render a
          
portVar :: Var
portVar = Var LocalVar Ir "Port"

-- hard stuff

getSoundSources :: Sco (Mix a) -> IO PreSndTab
getSoundSources = un

tableSoundSources :: PreSndTab -> InstrTab SndSrc
tableSoundSources = un

getMixing :: PreSndTab -> Sco (Mix a) -> IO (InstrTab Mixing)
getMixing = un




