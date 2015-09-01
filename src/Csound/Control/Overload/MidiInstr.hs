{-# Language TypeFamilies, FlexibleInstances, FlexibleContexts #-}
module Csound.Control.Overload.MidiInstr(
    MidiInstr(..)
) where

import Csound.Typed
import Csound.Typed.Opcode

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi msg 1, cpsmidi msg)

-------------------------------------------------------------------------------

-- | Converts a value to the midi-instrument. It's used with the functions 'Csound.Base.midi', 'Csound.Base.midin'.
class MidiInstr a where
    type MidiInstrOut a :: *

    onMsg :: a -> Msg -> SE (MidiInstrOut a)

-- just sig

instance MidiInstr (Msg -> Sig) where
    type MidiInstrOut (Msg -> Sig) = Sig

    onMsg f = return . f

instance MidiInstr (Msg -> (Sig, Sig)) where
    type MidiInstrOut (Msg -> (Sig, Sig)) = (Sig, Sig)

    onMsg f = return . f

instance MidiInstr (Msg -> (Sig, Sig, Sig)) where
    type MidiInstrOut (Msg -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = return . f

instance MidiInstr (Msg -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (Msg -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = return . f

-- se sig

instance MidiInstr (Msg -> SE Sig) where
    type MidiInstrOut (Msg -> SE Sig) = Sig

    onMsg f = f

instance MidiInstr (Msg -> SE (Sig, Sig)) where
    type MidiInstrOut (Msg -> SE (Sig, Sig)) = (Sig, Sig)

    onMsg f = f

instance MidiInstr (Msg -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut (Msg -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = f

instance MidiInstr (Msg -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (Msg -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = f

-- by (Sig, Sig)

sig2 :: Msg -> (Sig, Sig)
sig2 msg = (sig amp, sig cps)
    where (amp, cps) = ampCps msg

instance MidiInstr ((Sig, Sig) -> Sig) where
    type MidiInstrOut ((Sig, Sig) -> Sig) = Sig

    onMsg f = return . f . sig2

instance MidiInstr ((Sig, Sig) -> (Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> (Sig, Sig)) = (Sig, Sig)

    onMsg f = return . f . sig2

instance MidiInstr ((Sig, Sig) -> (Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = return . f . sig2

instance MidiInstr ((Sig, Sig) -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = return . f . sig2

-- se sig

instance MidiInstr ((Sig, Sig) -> SE Sig) where
    type MidiInstrOut ((Sig, Sig) -> SE Sig) = Sig

    onMsg f = f . sig2

instance MidiInstr ((Sig, Sig) -> SE (Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> SE (Sig, Sig)) = (Sig, Sig)

    onMsg f = f . sig2

instance MidiInstr ((Sig, Sig) -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = f . sig2

instance MidiInstr ((Sig, Sig) -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, Sig) -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = f . sig2

-- by Sig / D

dsig :: Msg -> (D, Sig)
dsig msg = (amp, sig cps)
    where (amp, cps) = ampCps msg

instance MidiInstr ((D, Sig) -> Sig) where
    type MidiInstrOut ((D, Sig) -> Sig) = Sig

    onMsg f = return . f . dsig

instance MidiInstr ((D, Sig) -> (Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> (Sig, Sig)) = (Sig, Sig)

    onMsg f = return . f . dsig

instance MidiInstr ((D, Sig) -> (Sig, Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = return . f . dsig

instance MidiInstr ((D, Sig) -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = return . f . dsig

-- se sig

instance MidiInstr ((D, Sig) -> SE Sig) where
    type MidiInstrOut ((D, Sig) -> SE Sig) = Sig

    onMsg f = f . dsig

instance MidiInstr ((D, Sig) -> SE (Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> SE (Sig, Sig)) = (Sig, Sig)

    onMsg f = f . dsig

instance MidiInstr ((D, Sig) -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = f . dsig

instance MidiInstr ((D, Sig) -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((D, Sig) -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = f . dsig

-- by Sig / D

sigd :: Msg -> (Sig, D)
sigd msg = (sig amp, cps)
    where (amp, cps) = ampCps msg

instance MidiInstr ((Sig, D) -> Sig) where
    type MidiInstrOut ((Sig, D) -> Sig) = Sig

    onMsg f = return . f . sigd

instance MidiInstr ((Sig, D) -> (Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> (Sig, Sig)) = (Sig, Sig)

    onMsg f = return . f . sigd

instance MidiInstr ((Sig, D) -> (Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = return . f . sigd

instance MidiInstr ((Sig, D) -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = return . f . sigd

-- se sig

instance MidiInstr ((Sig, D) -> SE Sig) where
    type MidiInstrOut ((Sig, D) -> SE Sig) = Sig

    onMsg f = f . sigd

instance MidiInstr ((Sig, D) -> SE (Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> SE (Sig, Sig)) = (Sig, Sig)

    onMsg f = f . sigd

instance MidiInstr ((Sig, D) -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = f . sigd

instance MidiInstr ((Sig, D) -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((Sig, D) -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = f . sigd

-- d2

d2 :: Msg -> (D, D)
d2 = ampCps

instance MidiInstr ((D, D) -> Sig) where
    type MidiInstrOut ((D, D) -> Sig) = Sig

    onMsg f = return . f . d2 

instance MidiInstr ((D, D) -> (Sig, Sig)) where
    type MidiInstrOut ((D, D) -> (Sig, Sig)) = (Sig, Sig)

    onMsg f = return . f . d2

instance MidiInstr ((D, D) -> (Sig, Sig, Sig)) where
    type MidiInstrOut ((D, D) -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = return . f . d2

instance MidiInstr ((D, D) -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((D, D) -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = return . f . d2

-- se sig

instance MidiInstr ((D, D) -> SE Sig) where
    type MidiInstrOut ((D, D) -> SE Sig) = Sig

    onMsg f = f . d2

instance MidiInstr ((D, D) -> SE (Sig, Sig)) where
    type MidiInstrOut ((D, D) -> SE (Sig, Sig)) = (Sig, Sig)

    onMsg f = f . d2

instance MidiInstr ((D, D) -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut ((D, D) -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f = f . d2

instance MidiInstr ((D, D) -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut ((D, D) -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f = f . d2

-- sig

instance MidiInstr (Sig -> Sig) where
    type MidiInstrOut (Sig -> Sig) = Sig

    onMsg f msg = return $ sig (ampmidi msg 1) * f (sig (cpsmidi msg))
    
instance MidiInstr (Sig -> (Sig, Sig)) where
    type MidiInstrOut (Sig -> (Sig, Sig)) = (Sig, Sig)

    onMsg f msg = return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2)
        where (a1, a2) = f (sig (cpsmidi msg))

instance MidiInstr (Sig -> (Sig, Sig, Sig)) where
    type MidiInstrOut (Sig -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f msg = return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2, sig (ampmidi msg 1) * a3)
        where (a1, a2, a3) = f (sig (cpsmidi msg))

instance MidiInstr (Sig -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (Sig -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f msg = return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2, sig (ampmidi msg 1) * a3, sig (ampmidi msg 1) * a4)
        where (a1, a2, a3, a4) = f (sig (cpsmidi msg))

    
instance MidiInstr (Sig -> SE Sig) where
    type MidiInstrOut (Sig -> SE Sig) = Sig

    onMsg f msg = do
        a1 <- f (sig (cpsmidi msg))
        return $ sig (ampmidi msg 1) * a1
    
instance MidiInstr (Sig -> SE (Sig, Sig)) where
    type MidiInstrOut (Sig -> SE (Sig, Sig)) = (Sig, Sig)

    onMsg f msg = do
        (a1, a2) <- f (sig (cpsmidi msg))
        return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2)

instance MidiInstr (Sig -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut (Sig -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f msg = do
        (a1, a2, a3) <- f (sig (cpsmidi msg))
        return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2, sig (ampmidi msg 1) * a3)

instance MidiInstr (Sig -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (Sig -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f msg = do
        (a1, a2, a3, a4) <- f (sig (cpsmidi msg))
        return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2, sig (ampmidi msg 1) * a3, sig (ampmidi msg 1) * a4)

-- d

instance MidiInstr (D -> Sig) where
    type MidiInstrOut (D -> Sig) = Sig

    onMsg f msg = return $ sig (ampmidi msg 1) * f (cpsmidi msg)
    
instance MidiInstr (D -> (Sig, Sig)) where
    type MidiInstrOut (D -> (Sig, Sig)) = (Sig, Sig)

    onMsg f msg = return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2)
        where (a1, a2) = f (cpsmidi msg)

instance MidiInstr (D -> (Sig, Sig, Sig)) where
    type MidiInstrOut (D -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f msg = return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2, sig (ampmidi msg 1) * a3)
        where (a1, a2, a3) = f (cpsmidi msg)

instance MidiInstr (D -> (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (D -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f msg = return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2, sig (ampmidi msg 1) * a3, sig (ampmidi msg 1) * a4)
        where (a1, a2, a3, a4) = f (cpsmidi msg)

instance MidiInstr (D -> SE Sig) where
    type MidiInstrOut (D -> SE Sig) = Sig

    onMsg f msg = do
        a1 <- f ((cpsmidi msg))
        return $ sig (ampmidi msg 1) * a1
    
instance MidiInstr (D -> SE (Sig, Sig)) where
    type MidiInstrOut (D -> SE (Sig, Sig)) = (Sig, Sig)

    onMsg f msg = do
        (a1, a2) <- f ((cpsmidi msg))
        return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2)

instance MidiInstr (D -> SE (Sig, Sig, Sig)) where
    type MidiInstrOut (D -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

    onMsg f msg = do
        (a1, a2, a3) <- f ((cpsmidi msg))
        return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2, sig (ampmidi msg 1) * a3)

instance MidiInstr (D -> SE (Sig, Sig, Sig, Sig)) where
    type MidiInstrOut (D -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

    onMsg f msg = do
        (a1, a2, a3, a4) <- f ((cpsmidi msg))
        return $ (sig (ampmidi msg 1) * a1, sig (ampmidi msg 1) * a2, sig (ampmidi msg 1) * a3, sig (ampmidi msg 1) * a4)


