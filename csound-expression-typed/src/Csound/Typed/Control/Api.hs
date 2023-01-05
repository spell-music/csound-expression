{-# Language ScopedTypeVariables #-}
module Csound.Typed.Control.Api(
    trigByName, trigByName_,
    trigByNameMidi, trigByNameMidi_,
    namedMonoMsg
) where

import Data.Boolean

import qualified Csound.Dynamic as D

import Csound.Typed.Types
import Csound.Typed.Control.Ref
import Csound.Typed.GlobalState
import Csound.Typed.GlobalState.Opcodes(eventi, Event(..), downsamp)
import Csound.Typed.InnerOpcodes
import Data.Text (Text)

import Csound.Typed.Plugins.TabQueue

-- | Creates an instrument that can be triggered by name with Csound API.
-- The arguments are determined from the structure of the input for the instrument.
--
-- With Csound API we can send messages
--
-- > i "name" time duration arg1 arg2 arg3
trigByName_ :: Arg a => Text -> (a -> SE ()) -> SE ()
trigByName_ name instr = geToSe $ saveNamedInstr name =<< (execSE $ instr toArg)

-- | Creates an instrument that can be triggered by name with Csound API.
-- The arguments are determined from the structure of the input for the instrument.
-- If we have a tuple of arguments: @(D, D, Tab)@
-- The would be rendered to instrument arguments that strts from @p4@.
-- @p1@ is the name of teh instrument, @p2@ is the start time of the note,
-- @p3@ is the duration of the note. Then @p4@ and @p5@ are going to be doubles and @p6@
-- is an integer that denotes a functional table.
trigByName  :: (Arg a, Sigs b) => Text -> (a -> SE b) -> SE b
trigByName name instr = do
    ref <- newClearableGlobalRef 0
    trigByName_ name (go ref)
    readRef ref
    where go ref x = mixRef ref =<< instr x


-- | It behaves just like the function @trigByNameMidi@. Only it doesn't produce an audio
-- signal. It performs some procedure on note on and stops doing the precedure on note off.
trigByNameMidi_ :: forall a . Arg a => Text -> ((D, D, a) -> SE ()) -> SE ()
trigByNameMidi_ name instr = do
    instrId <- geToSe $ saveInstr (instr toArg)
    trigByName_ name (go instrId)
    where
        go :: D.InstrId -> (D, D, D, a) -> SE ()
        go instrId (noteFlag, pch, vol, other) = fromDep_ $ hideGEinDep $ do
            pchExpr      <- toGE pch
            let instrIdExpr = D.instrIdE instrId + pchExpr / 1000
            noteFlagExpr <- toGE noteFlag
            args <- fromTuple (pch, vol, other)
            return $ do
                    D.when1 D.IfIr (noteFlagExpr ==* 1) $ do
                        eventi (Event instrIdExpr 0 (-1) args)
                    D.when1 D.IfIr (noteFlagExpr ==* 0) $ do
                        eventi (Event (negate instrIdExpr) 0 0 args)
                    turnoff

-- | Creates an instrument that can be triggered by name with Csound API.
--
-- It's intended to be used like a midi instrument. It simulates a simplified midi protocol.
-- We can trigger notes:
--
-- > i "givenName" delay duration 1 pitchKey volumeKey auxParams     -- note on
-- > i "givenName" delay duration 0 pitchKey volumeKey auxParams     -- note off
--
-- The arguments are
--
-- > trigByNameMidi name instrument
--
-- The instrument takes a triplet of @(pitchKey, volumeKey, auxilliaryTuple)@.
-- The order does matter. Please don't pass the @volumeKey@ as the first argument.
-- The instrument expects the pitch key to be a first argument.

-- Under the hood
-- it creates held notes that are indexed by pitch. If you know the Csound it creates
-- the notes with indexes:
--
-- > i 18.pitchKey
--
-- Here the 18 is some generated integer index. And then on receiving a note a note off message for the specific key the
-- Csound procedure invokes:
--
-- > turnoff 18.pitchKey
trigByNameMidi  :: (Arg a, Sigs b) => Text -> ((D, D, a) -> SE b) -> SE b
trigByNameMidi name instr = do
    ref <- newClearableGlobalRef 0
    trigByNameMidi_ name (go ref)
    readRef ref
    where go ref x = mixRef ref =<< instr x

namedMonoMsg :: Text -> SE MonoArg
namedMonoMsg name = do
    refPch <- newGlobalRef 0
    refVol <- newGlobalRef 0
    tab <- newGlobalTab 24
    let onFlag = tabQueue2_hasElements tab
    trigByNameMidiCbk name (onNote tab) (offNote tab)
    when1 onFlag $ do
        let (pch, vol) = tabQueue2_readLastElement tab
        writeRef refPch pch
        writeRef refVol vol
    when1 (notB onFlag) $ do
        writeRef refVol 0
    pchKey <- readRef refPch
    volKey <- readRef refVol
    let kgate = ifB onFlag 1 0
        kamp = downsamp' volKey
        kcps = downsamp' pchKey
        trig = changed [kamp, kcps]
    return $ MonoArg kamp kcps kgate trig
    where
        onNote = tabQueue2_append
        offNote tab (pch, _vol) = tabQueue2_delete tab pch

trigByNameMidiCbk :: Text -> ((D, D) -> SE ())  -> ((D, D) -> SE ()) -> SE ()
trigByNameMidiCbk name noteOn noteOff =
    trigByName_ name go
    where
        go :: (D, D, D) -> SE ()
        go (noteFlag, pch, vol) = do
            whenD1 (noteFlag ==* 1) $ noteOn (pch, vol)
            whenD1 (noteFlag ==* 0) $ noteOff (pch, vol)
            SE turnoff

downsamp' :: Sig -> Sig
downsamp' a = fromGE $ do
    a' <- toGE a
    return $ downsamp a'

