-- | Essential opcodes
module Csound.Typed.Core.Opcodes
  ( outs
  , oscil
  , linseg
--  , event
--  , event_i
  , diskin2
  , schedule
  , schedulek
  ) where

import Csound.Dynamic hiding (InstrId (..))
import Csound.Typed.Core.Types hiding (setRate)
import Control.Monad.Trans.Class (lift)

-- |
-- Writes stereo audio data to an external device or stream.
--
-- >  outs  asig1, asig2
--
-- csound doc: <http://csound.com/docs/manual/outs.html>
outs :: (Sig, Sig) -> SE ()
outs (b1, b2) = SE $ (depT_ =<<) $ lift $ f <$> toE b1 <*> toE b2
    where f a1 a2 = opcs "outs" [(Xr,[Ar,Ar])] [a1,a2]

-- |
-- A simple oscillator.
--
-- oscil reads table ifn sequentially and repeatedly at a frequency xcps. The amplitude is scaled by xamp.
--
-- > ares  oscil  xamp, xcps [, ifn, iphs]
-- > kres  oscil  kamp, kcps [, ifn, iphs]
--
-- csound doc: <http://csound.com/docs/manual/oscil.html>
oscil ::  Sig -> Sig -> Tab -> Sig
oscil b1 b2 b3 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
    where f a1 a2 a3 = opcs "oscil" [(Ar,[Xr,Xr,Ir,Ir]),(Kr,[Kr,Kr,Ir,Ir])] [a1,a2,a3]

-- |
-- Trace a series of line segments between specified points.
--
-- > ares  linseg  ia, idur1, ib [, idur2] [, ic] [...]
-- > kres  linseg  ia, idur1, ib [, idur2] [, ic] [...]
--
-- csound doc: <http://csound.com/docs/manual/linseg.html>
linseg ::  [D] -> Sig
linseg b1 = Sig $ f <$> mapM unD b1
    where f a1 = setRate Kr $ opcs "linseg" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])

{-
-- |
-- Generates a score event from an instrument.
--
-- >  event  "scorechar", kinsnum, kdelay, kdur, [, kp4] [, kp5] [, ...]
-- >  event  "scorechar", "insname", kdelay, kdur, [, kp4] [, kp5] [, ...]
--
-- csound doc: <http://csound.com/docs/manual/event.html>
event :: Tuple a => Str -> InstrId a -> Sig -> Sig -> a -> SE ()
event b1 instrId start dur args = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> toE instrId <*> toE start <*> toE dur <*> fromTuple args
    where f a1 a2 a3 a4 a5 = opcs "event" [(Xr,[Sr,getInstrIdRate instrId] ++ (repeat Kr))] ([a1,a2,a3,a4] ++ a5)

-- |
-- Generates a score event from an instrument.
--
-- >  event_i  "scorechar", iinsnum, idelay, idur, [, ip4] [, ip5] [, ...]
-- >  event_i  "scorechar", "insname", idelay, idur, [, ip4] [, ip5] [, ...]
--
-- csound doc: <http://csound.com/docs/manual/event_i.html>
event_i ::  Arg a => Str -> InstrId a -> D -> D -> a -> SE ()
event_i b1 instrId b3 b4 args = SE $ (depT_ =<<) $ lift $ f <$> unStr b1 <*> toE instrId <*> unD b3 <*> unD b4 <*> fromTuple args
    where f a1 a2 a3 a4 a5 = opcs "event_i" [(Xr,[Sr,getInstrIdRate instrId] ++ (repeat Ir))] ([a1,a2,a3,a4] ++ a5)
-}

diskin2 :: Tuple a => Str -> a
diskin2 b1 = pureTuple $ f <$> unStr b1
    where f a1 = mopcs "diskin2" ((repeat Ar),[Sr,Kr,Ir,Ir,Ir,Ir,Ir,Ir]) [a1]

schedule :: forall args ty . (Val ty, Arg args) => ProcId ty args -> D -> D -> args -> SE ()
schedule instrId start dur args = SE $ (depT_ =<<) $ lift $ f <$> fromTuple (instrId, start, dur, args)
    where f as = opcs "schedule" [(Xr, tupleRates @(ProcId ty args, D, D, args))] as

schedulek :: forall a ty . (Val ty, Arg a) => ProcId ty a -> D -> D -> a -> SE ()
schedulek instrId start dur args = SE $ (depT_ =<<) $ lift $ f <$> fromTuple (instrId, start, dur, args)
    where f as = opcs "schedulek" [(Xr, tupleRates @(K (ProcId ty a, D, D, a)))] as
