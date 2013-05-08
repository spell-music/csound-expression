-- | Opcodes for routing the signals
module Csound.Render.Channel (
    InstrId,
    -- * renders instruments to expressions
    instrExp, mixerExp, masterMixerExp,
    -- * midi instruments
    midiInits, midiReset,
    -- * master output
    masterOuts, outs, midiOuts, midiVar, 
    -- * channel opcodes
    chnVar, chnName, 
    chnmix, chnget, chnclear,
    chnUpdateStmt
) where

import Control.Monad(zipWithM_)
import Data.Foldable(foldMap)
import Data.List(transpose)

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Mix
import Csound.Exp.Cons(opc1, opc2, opcs)
import Csound.Opcode(clip, zeroDbfs, sprintf)
import Csound.Render.Pretty(verbatimLines)
import Csound.Render.Pretty

-----------------------------------------------------------
-- simple instrument trigered with score

-- How to render an instrument
instrExp :: InstrId -> Instr -> E
instrExp instrId x = execSE $ instrOut =<< instrBody x
    where instrOut = case instrMidi x of
            -- 4 + arity because there are 3 first arguments (instrId, start, dur) and arity params comes next
            Nothing                     -> outs (4 + arityIns (instrArity x))   
            Just (midiType, channel)    -> midiOuts instrId


-----------------------------------------------------------
-- mixer instruments

masterMixerExp :: Mixer -> MixerExp
masterMixerExp = mixerExpGen masterOuts

mixerExp :: Mixer -> MixerExp
mixerExp = mixerExpGen (outs 4) -- for mixing instruments we expect the port number to be the fourth parameter

mixerExpGen :: ([Sig] -> SE ()) -> Mixer -> MixerExp
mixerExpGen formOuts (Mixer arity effect sco) = MixerExp exp sco
    where exp = execSE $ formOuts . mixMidis midiNotes =<< effect =<< ins arity
          midiNotes = foldMap getMidiFromMixNote sco

midiInits :: [MidiInstrParams] -> Doc
midiInits = vcat . fmap initMidiVar . (getMidiVars =<< )

midiReset :: InstrId -> [MidiInstrParams] -> Doc
midiReset n = ppInstr n . fmap reset . (getMidiVars =<< ) 
    where reset v = ppVar v $= int 0

initMidiVar :: Var -> Doc
initMidiVar a = ppOpc (ppVar a) "init" [int 0]

resetMidiVarInstr :: [Var] -> E
resetMidiVarInstr vs = execSE $ mapM_ (flip writeVar (0 :: Sig)) vs

-----------------------------------------------------------
-- midi instrument

mixMidis :: [MidiInstrParams] -> [Sig] -> [Sig]
mixMidis ms sigs 
    | null ms   = sigs
    | otherwise = zipWith (+) midiSums sigs
    where midiSums = fmap sum $ transpose $ fmap (fmap readVar . getMidiVars) ms

getMidiFromMixNote :: MixerNote -> [MidiInstrParams] 
getMidiFromMixNote x = case x of
    MidiNote a -> [a]
    _ -> []

getMidiVars :: MidiInstrParams -> [Var]
getMidiVars (MidiInstrParams arity instrId _ _) = fmap (midiVar instrId) [1 .. arityOuts arity]

---------------------------------------------------------
-- master instrument output

masterOuts :: [Sig] -> SE ()
masterOuts = outs . clipByMax
    where outs xs = se_ $ case xs of
              a:[] -> opc1 "out" [(Xr, [Ar])] a
              _    -> opcs "outs" [(Xr, repeat Ar)] xs    

clipByMax :: [Sig] -> [Sig]
clipByMax = fmap clip'
    where clip' x = clip x 0 zeroDbfs

-- other outputs

midiOuts :: InstrId -> [Sig] -> SE ()
midiOuts instrId as = zipWithM_ (\portId sig -> writeVar (midiVar instrId portId) sig) [1 .. ] as

outs :: Int -> [Sig] -> SE ()
outs readChnId sigs = zipWithM_ (out readChnId) [1 .. ] sigs
    where out readChnId n sig = chnmix sig $ chnName n (p readChnId) 

midiVar :: InstrId -> Int -> Var
midiVar instrId portId = Var GlobalVar Ar ("midi_" ++ show instrId ++ "_" ++ show portId)

-- inputs

ins :: Arity -> SE [Sig]
ins arity = mapM in_ [1 .. arityIns arity] 
    where in_ n = do
              sig <- chnget name
              chnclear name
              return sig    
              where name = chnName n $ readVar chnVar

----------------------------------------------------------
-- channels
       
chnVar :: Var
chnVar = Var LocalVar Ir "Port"

chnName :: Int -> D -> Str
chnName name = sprintf formatString . return
    where formatString = str $ 'p' : show name ++ "_" ++ "%d"

chnmix :: Sig -> Str -> SE ()
chnmix a b = se_ $ opc2 "chnmix" [(Xr, [Ar, Sr])] a b

chnget :: Str -> SE Sig
chnget a = se $ opc1 "chnget" [(Ar, [Sr])] a

chnclear :: Str -> SE ()
chnclear a = se_ $ opc1 "chnclear" [(Xr, [Sr])] a


chnUpdateStmt = verbatimLines [
    "giPort init 1",
    "opcode FreePort, i, 0",
    "xout giPort",
    "giPort = giPort + 1",
    "endop"]

