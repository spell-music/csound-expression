-- | Opcodes for routing the signals
module Csound.Render.Channel (
    InstrId,
    -- * renders instruments to expressions
    instrExp, mixerExp, masterExp,
    -- * master output
    masterOuts, outs,
    -- * master inputs
    ins,
    -- * channel opcodes
    chnVar, chnName, 
    chnmix, chnget, chnclear,
    chnUpdateStmt, chnUpdateOpcodeName,
    -- * trigger an instrument
    event, eventWithChannel, instrOn, instrOff
) where

import Control.Monad(zipWithM_)
import Data.Foldable(foldMap)
import Data.List(transpose)

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Tuple(Out)
import Csound.Exp.Arg(Arg, toNote)
import Csound.Exp.Cons(opc0, opc1, opc2, opcs, spec1)
import Csound.Opcode(clip, zeroDbfs, sprintf)
import Csound.Render.Pretty(verbatimLines)

-----------------------------------------------------------
-- simple instrument trigered with score

-- How to render an instrument
masterExp, mixerExp :: SE [Sig] -> E

-- 4 + arity because there are 3 first arguments (instrId, start, dur) and arity params comes next
masterExp  = instrExpGen masterOuts
mixerExp   = instrExpGen (outs 4) -- for mixing instruments we expect the port number to be the fourth parameter

instrExp :: Int -> SE [Sig] -> E
instrExp insArity = instrExpGen (outs (4 + insArity))

instrExpGen :: ([Sig] -> SE ()) -> SE [Sig] -> E
instrExpGen formOuts instrBody = execSE $ formOuts =<< instrBody


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

outs :: Int -> [Sig] -> SE ()
outs readChnId sigs = zipWithM_ (out readChnId) [1 .. ] sigs
    where out readChnId n sig = chnmix sig $ chnName n (p readChnId) 

-- inputs

ins :: Int -> SE [Sig]
ins n = mapM in_ [1 .. n] 
    where in_ n = do
              let name = chnName n $ readVar chnVar
              sig <- chnget name
              chnclear name
              return sig    

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

