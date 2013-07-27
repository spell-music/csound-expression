-- | Opcodes for routing the signals
module Csound.Render.Channel (
    clip, zeroDbfs, sprintf, ihold, turnoff, random, follow, changed,
    masterOuts,
    -- * channel opcodes
    chnVar, chnName, 
    chnmix, chnget, chnclear,
    chnUpdateStmt, chnUpdateOpcodeName, ppFreeChnStmt,
    freeChn, 
    -- * trigger an instrument
    event, eventWithChannel, instrOn, instrOff,
    -- * set gui value
    flSetVal, flPrintk2
) where

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.Arg(Arg)
import Csound.Exp.Tuple(Out(..), CsdTuple, fromCsdTuple)
import Csound.Exp.Cons(opc0, opc1, opc2, opc3, opcs)
import Csound.Render.Pretty(Doc, verbatimLines, ppOpc, ppVar)   

-- | Clips an a-rate signal to a predefined limit, in a “soft” manner, using one of three methods. 
--
-- > ares clip asig, imeth, ilimit [, iarg]
--
-- doc: <http://www.csounds.com/manual/html/clip.html>
clip :: Sig -> D -> D -> Sig
clip = opc3 "clip" [(Ar, [Ar, Ir, Ir])]

-- | Reads @0dbfs@ value.
zeroDbfs :: D
zeroDbfs = (setRate Ir :: E -> D) $ readOnlyVar (VarVerbatim Ir "0dbfs")

-- | sprintf write printf-style formatted output to a string variable, similarly to the C function sprintf(). sprintf runs at i-time only. 
--
-- > Sdst sprintf Sfmt, xarg1[, xarg2[, ... ]]
--
-- doc: <http://www.csounds.com/manual/html/sprintf.html>
sprintf :: Str -> [D] -> Str
sprintf a1 a2 = opcs "sprintf" [(Sr, Sr:repeat Ir)] (toE a1 : map toE a2)

-- | Causes a finite-duration note to become a “held” note.
--
-- >    ihold
--
-- doc: <http://www.csounds.com/manual/html/ihold.html>
ihold :: SE ()
ihold = se_ $ opc0 "ihold" [(Xr, [])]

-- | Enables an instrument to turn itself off.
--
-- >    turnoff
--
-- doc: <http://www.csounds.com/manual/html/turnoff.html>
turnoff :: SE ()
turnoff = se_ $ opc0 "turnoff" [(Xr, [])]

random :: Sig -> Sig -> SE Sig
random xMin xMax = se $ opc2 "random" [(Kr, [Kr, Kr])] xMin xMax

-- amplitude follower
follow :: Sig -> D -> Sig
follow = opc2 "follow" [(Ar, [Kr, Ir])]

-- | This opcode outputs a trigger signal that informs when any one of its k-rate 
-- arguments has changed. Useful with valuator widgets or MIDI controllers.
--
-- > ktrig changed kvar1 [, kvar2,..., kvarN]
--
-- doc: <http://www.csounds.com/manual/html/changed.html>
changed :: [Ksig] -> Ksig
changed = opcs "changed" [(Kr, repeat Kr)]


---------------------------------------------------------
-- master instrument output

masterOuts :: (Out a) => a -> SE ()
masterOuts outSigs = outs . clipByMax =<< toOut outSigs
    where outs xs = case xs of
              []   -> return ()
              a:[] -> se_ $ opc1 "out" [(Xr, [Ar])] a
              _    -> se_ $ opcs "outs" [(Xr, repeat Ar)] xs    

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

chnUpdateStmt :: Doc
chnUpdateStmt = verbatimLines [
    "giPort init 1",
    "opcode " ++ chnUpdateOpcodeName ++ ", i, 0",
    "xout giPort",
    "giPort = giPort + 1",
    "endop"]

ppFreeChnStmt :: Doc
ppFreeChnStmt = ppOpc (ppVar chnVar) chnUpdateOpcodeName []

chnUpdateOpcodeName :: String
chnUpdateOpcodeName = "FreePort"

freeChn :: SE D
freeChn = se $ opc0 "FreePort" [(Ir, [])]

-------------------------------------------------------------
-- notes

event :: (CsdTuple a, Arg a) => InstrId -> D -> D -> a -> SE ()
event instrId start dur arg = se_ $ opcs "event" [(Xr, repeat Kr)] argExp
    where argExp :: [E]
          argExp = fromCsdTuple (str "i", double $ fromIntegral $ instrIdCeil $ instrId, start, dur, arg) 

eventWithChannel :: (CsdTuple a, Arg a) => InstrId -> D -> D -> a -> D -> SE ()
eventWithChannel instrId start dur arg chn = event instrId start dur (arg, chn)

instrOn :: (CsdTuple a, Arg a) => InstrId -> a -> D -> SE ()
instrOn instrId arg chn = eventWithChannel instrId 0 (-1) arg chn

instrOff :: InstrId -> SE ()
instrOff instrId = event (toNegative instrId) 0 (-1) ()
    where toNegative a = a { instrIdCeil = negate $ abs $ instrIdCeil a }

-------------------------------------------------------------
-- set gui value

flSetVal :: Sig -> Sig -> D -> SE ()
flSetVal trig val handle = se_ $ opc3 "FLsetVal" [(Xr, [Kr, Kr, Ir])] trig val handle

flPrintk2 :: Sig -> D -> SE ()
flPrintk2 val handle = se_ $ opc2 "FLprintk2" [(Xr, [Kr, Ir])] val handle



