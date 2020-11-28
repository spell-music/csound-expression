module Csound.Typed.Types.MixSco(
    M(..), CsdEventList, csdEventListDur, csdEventListNotes,
    delayAndRescaleCsdEventListM, renderMixSco, renderMixSco_
) where

import Control.Applicative
import Control.Monad

import Csound.Dynamic hiding (int)
import Csound.Typed.GlobalState.Elements
import Csound.Typed.GlobalState.Opcodes
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.Control.Ref
import Csound.Typed.Types.Prim

import qualified Temporal.Media as T

type CsdEventList a = T.Track Sig a

csdEventListNotes :: CsdEventList a -> [(Sig, Sig, a)]
csdEventListNotes a = fmap (\(T.Event start dur content) -> (start, dur, content)) $ T.render a

csdEventListDur :: CsdEventList a -> Sig
csdEventListDur = T.dur

rescaleCsdEventList :: Sig -> CsdEventList a -> CsdEventList a
rescaleCsdEventList = T.str

delayCsdEventList :: Sig -> CsdEventList a -> CsdEventList a
delayCsdEventList = T.del


type TupleMonoArg = (E,E,E,E)
type RawMonoInstr = TupleMonoArg -> Dep [E]

data M
    = Snd InstrId (CsdEventList [E])
    | MonoSnd { monoSndInstr :: InstrId, monoSndArgs :: InstrId, monoSndNotes :: (CsdEventList [E]) }
    | Eff InstrId (CsdEventList M) Int

delayAndRescaleCsdEventListM :: CsdEventList M -> CsdEventList M
delayAndRescaleCsdEventListM = delayCsdEventListM . rescaleCsdEventListM

delayCsdEventListM :: CsdEventList M -> CsdEventList M
delayCsdEventListM = T.mapEvents delayCsdEventM

delayCsdEventM :: T.Event Sig M -> T.Event Sig M
delayCsdEventM (T.Event start dur evt) = T.Event start dur (phi evt)
    where phi x = case x of
            Snd n evts                  -> Snd n $ delayCsdEventList start evts
            MonoSnd instrId argId evts  -> MonoSnd instrId argId  $ delayCsdEventList start evts
            Eff n evts arityIn          -> Eff n (delayCsdEventListM $ delayCsdEventList start evts) arityIn

rescaleCsdEventListM :: CsdEventList M -> CsdEventList M
rescaleCsdEventListM = T.mapEvents rescaleCsdEventM

rescaleCsdEventM :: T.Event Sig M -> T.Event Sig M
rescaleCsdEventM (T.Event start dur evt) = T.Event start dur (phi evt)
    where phi x = case x of
            Snd n evts                  -> Snd n $ rescaleCsdEventList (dur/localDur) evts
            MonoSnd instrId argId evts  -> MonoSnd instrId argId  $ rescaleCsdEventList (dur/localDur) evts
            Eff n evts arityIn          -> Eff n (rescaleCsdEventListM $ rescaleCsdEventList (dur/localDur) evts) arityIn
            where localDur = case x of
                    Snd _ evts       -> csdEventListDur evts
                    MonoSnd _ _ evts -> csdEventListDur evts
                    Eff _ evts _     -> csdEventListDur evts

renderMixSco :: Int -> CsdEventList M -> Dep [E]
renderMixSco arity evts = do
    chnId <- chnRefAlloc arity
    aliveCountRef <- unSE $ newRef (10 :: D)
    go aliveCountRef chnId evts
    readChn chnId
    where
        go :: Ref D -> ChnRef -> CsdEventList M -> Dep ()
        go aliveCountRef outId xs = do
            mapM_ (onEvent aliveCountRef outId) notes
            unSE $ writeRef aliveCountRef $ int $ 2 * length notes
            aliveCount <- unSE $ readRef aliveCountRef
            hideGEinDep $ liftA2 masterUpdateChnAlive (return chnId) $ toGE aliveCount
            where
                notes = csdEventListNotes xs
                chnId = outId

        onEvent :: Ref D -> ChnRef -> (Sig, Sig, M) -> Dep ()
        onEvent aliveCountRef outId (start, dur, x) = case x of
            Snd instrId es          -> onSnd aliveCountRef instrId outId es
            MonoSnd instr arg es    -> onMonoSnd instr arg start dur outId es
            Eff instrId es arityIn  -> onEff aliveCountRef instrId start dur outId es arityIn

        onSnd _ instrId outId es = forM_ (csdEventListNotes es) $ \(start, dur, args) ->
            mkEvent instrId start dur (args ++ [chnRefId outId])

        onEff aliveCountRef instrId start dur outId es arityIn = do
            inId <- chnRefAlloc arityIn
            mkEvent instrId start dur [chnRefId inId, chnRefId outId]
            go aliveCountRef inId es

        onMonoSnd instrId argId start dur outId es = do
            inId <- chnRefAlloc arityMonoIn

            forM_ (csdEventListNotes es) $ \(startLocal, durLocal, args) ->
                mkEvent argId startLocal durLocal (args ++ [chnRefId inId])

            mkEvent instrId start dur [chnRefId inId, chnRefId outId]
            where arityMonoIn = 3


renderMixSco_ :: CsdEventList M -> Dep ()
renderMixSco_ evts = mapM_ onEvent $ csdEventListNotes evts
    where
        onEvent :: (Sig, Sig, M) -> Dep ()
        onEvent (start, dur, x) = case x of
            Snd instrId es       -> onSnd instrId es
            MonoSnd instr arg es -> onMonoSnd instr arg es
            Eff instrId es _     -> onEff instrId start dur es

        onSnd instrId es = forM_ (csdEventListNotes es) $ \(start, dur, args) ->
            mkEvent instrId start dur args

        onEff instrId start dur es = do
            mkEvent instrId start dur []
            renderMixSco_ es

        onMonoSnd instr arg es = undefined


mkEvent :: InstrId -> Sig -> Sig -> [E] -> Dep ()
mkEvent instrId startD durD args =  hideGEinDep $ do
        start <- toGE startD
        dur   <- toGE durD
        return $ event_i $ Event (primInstrId instrId) start dur args
