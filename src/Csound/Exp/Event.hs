{-# Language GADTs, TypeFamilies #-}
module Csound.Exp.Event where

import Data.Monoid
import Temporal.Music.Score(Score)

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Cons(spec1)
import Csound.Exp.Logic
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Exp.Mix
import Csound.Exp.Logic
import Csound.Exp.GE
import Csound.Exp.SE
import Csound.Exp.Instr
import qualified Csound.Render.IndexMap as DM

type Bam a = a -> SE ()
type Trig = Evt ()

newtype Evt a = Evt { runEvt :: Bam a -> SE () }

trigger :: BoolSig -> Evt ()
trigger cond = Evt $ \bam -> when cond $ bam ()

instance Functor Evt where
    fmap f evt = Evt $ \bam -> runEvt evt (bam . f)

instance Monoid (Evt a) where
    mempty = Evt $ const $ return ()
    mappend a b = Evt $ \bam -> runEvt a bam >> runEvt b bam

filterEvt :: (a -> BoolD) -> Evt a -> Evt a
filterEvt pred evt = Evt $ \bam -> runEvt evt $ \a ->
    when (toBoolSig $ pred a) $ bam a

accumEvt :: (CsdTuple s) => s -> (a -> s -> (b, s)) -> Evt a -> Evt b
accumEvt s0 update evt = Evt $ \bam -> do
    (readSt, writeSt) <- initState s0
    runEvt evt $ \a -> do
        s1 <- readSt
        let (b, s2) = update a s1
        bam b

snapshot :: (CsdTuple a) => (Snap a -> b -> c) -> a -> Evt b -> Evt c
snapshot f sig evt = Evt $ \bam -> runEvt evt $ \a -> 
    bam (f (readSnap sig) a)

toBoolSig :: BoolD -> BoolSig
toBoolSig = undefined

initState :: CsdTuple s => s -> SE (SE s, s -> SE ())
initState s0 = undefined

readSnap :: CsdTuple a => a -> Snap a
readSnap = undefined

-------------------------------------------------------------------
-- snap 

type family Snap a :: *

type instance Snap D   = D
type instance Snap Str = Str
type instance Snap Tab = Tab

type instance Snap Sig = D

type instance Snap (a, b) = (Snap a, Snap b)
type instance Snap (a, b, c) = (Snap a, Snap b, Snap c)
type instance Snap (a, b, c, d) = (Snap a, Snap b, Snap c, Snap d)

--------------------------------------------------
--

stepper :: CsdTuple a => Evt a -> SE (NoSE a)
stepper = undefined

schedule :: (Arg a, Out b) => (a -> b) -> Evt (D, a) -> GE (NoSE b)
schedule instr trigger = undefined

toggle :: (Arg a, Out b) => (a -> b) -> Evt a -> Evt c -> GE (NoSE b)
toggle = undefined

