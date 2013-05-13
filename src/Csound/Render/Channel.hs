-- | Opcodes for routing the signals
module Csound.Render.Channel (
    masterOuts,
    -- * channel opcodes
    chnVar, chnName, 
    chnmix, chnget, chnclear,
    chnUpdateStmt, chnUpdateOpcodeName,
    -- * trigger an instrument
    event, eventWithChannel, instrOn, instrOff
) where

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Tuple(Out)
import Csound.Exp.Arg(Arg, toNote)
import Csound.Exp.Cons(opc0, opc1, opc2, opcs, spec1)
import Csound.Opcode(clip, zeroDbfs, sprintf)
import Csound.Render.Pretty(verbatimLines)

   
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
    "opcode " ++ chnUpdateOpcodeName ++ ", i, 0",
    "xout giPort",
    "giPort = giPort + 1",
    "endop"]

chnUpdateOpcodeName = "FreePort"

freeChn :: SE D
freeChn = se $ opc0 "FreePort" [(Ir, [])]

readChn :: Out b => D -> SE b
readChn chn = undefined


-------------------------------------------------------------
-- notes

event :: Arg a => InstrId -> D -> D -> a -> SE ()
event instrId start dur arg = se_ $ opcs "event" [(Xr, repeat Ir)] argExp
    where argExp :: [E]
          argExp = fmap prim $ toNote (str "i", start, dur, arg) 

eventWithChannel :: Arg a => InstrId -> D -> D -> a -> D -> SE ()
eventWithChannel instrId start dur arg chn = event instrId start dur (arg, chn)

instrOn :: Arg a => InstrId -> a -> D -> SE ()
instrOn instrId arg chn = eventWithChannel instrId 0 (-1) arg chn

instrOff :: InstrId -> SE ()
instrOff instrId = event (toNegative instrId) 0 (-1) ()
    where toNegative a = a { instrIdCeil = negate $ abs $ instrIdCeil a }

