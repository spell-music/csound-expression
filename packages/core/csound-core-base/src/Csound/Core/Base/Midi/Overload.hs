{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Csound.Core.Base.Midi.Overload (
  MidiInstr (..),
  MidiInstrTemp (..),
) where

import Data.Kind (Type)

import Csound.Core.Base.Midi.Internal
import Csound.Core.Opcode
import Csound.Core.Types

import Csound.Core.Base.Tuning

ampCps :: Msg -> (D, D)
ampCps msg = (ampmidi 1, cpsmidi)

-- | Midi message convertion to Hz with custom temperament.
cpsmidi' :: Temp -> Msg -> D
cpsmidi' (Temp t) msg = cpstmid t

ampCps' :: Temp -> Msg -> (D, D)
ampCps' temp msg = (ampmidi 1, cpsmidi' temp msg)

-------------------------------------------------------------------------------

-- | Converts a value to the midi-instrument. It's used with the functions 'Csound.Base.midi', 'Csound.Base.midin'.
class MidiInstr a where
  type MidiInstrOut a :: Type

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
sig2 msg = (toSig amp, toSig cps)
  where
    (amp, cps) = ampCps msg

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
dsig msg = (amp, toSig cps)
  where
    (amp, cps) = ampCps msg

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
sigd msg = (toSig amp, cps)
  where
    (amp, cps) = ampCps msg

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

  onMsg f msg = return $ toSig (ampmidi 1) * f (toSig (cpsmidi))

instance MidiInstr (Sig -> (Sig, Sig)) where
  type MidiInstrOut (Sig -> (Sig, Sig)) = (Sig, Sig)

  onMsg f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2)
    where
      (a1, a2) = f (toSig (cpsmidi))

instance MidiInstr (Sig -> (Sig, Sig, Sig)) where
  type MidiInstrOut (Sig -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

  onMsg f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3)
    where
      (a1, a2, a3) = f (toSig (cpsmidi))

instance MidiInstr (Sig -> (Sig, Sig, Sig, Sig)) where
  type MidiInstrOut (Sig -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

  onMsg f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3, toSig (ampmidi 1) * a4)
    where
      (a1, a2, a3, a4) = f (toSig (cpsmidi))

instance MidiInstr (Sig -> SE Sig) where
  type MidiInstrOut (Sig -> SE Sig) = Sig

  onMsg f msg = do
    a1 <- f (toSig (cpsmidi))
    return $ toSig (ampmidi 1) * a1

instance MidiInstr (Sig -> SE (Sig, Sig)) where
  type MidiInstrOut (Sig -> SE (Sig, Sig)) = (Sig, Sig)

  onMsg f msg = do
    (a1, a2) <- f (toSig (cpsmidi))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2)

instance MidiInstr (Sig -> SE (Sig, Sig, Sig)) where
  type MidiInstrOut (Sig -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

  onMsg f msg = do
    (a1, a2, a3) <- f (toSig (cpsmidi))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3)

instance MidiInstr (Sig -> SE (Sig, Sig, Sig, Sig)) where
  type MidiInstrOut (Sig -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

  onMsg f msg = do
    (a1, a2, a3, a4) <- f (toSig (cpsmidi))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3, toSig (ampmidi 1) * a4)

-- d

instance MidiInstr (D -> Sig) where
  type MidiInstrOut (D -> Sig) = Sig

  onMsg f msg = return $ toSig (ampmidi 1) * f (cpsmidi)

instance MidiInstr (D -> (Sig, Sig)) where
  type MidiInstrOut (D -> (Sig, Sig)) = (Sig, Sig)

  onMsg f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2)
    where
      (a1, a2) = f (cpsmidi)

instance MidiInstr (D -> (Sig, Sig, Sig)) where
  type MidiInstrOut (D -> (Sig, Sig, Sig)) = (Sig, Sig, Sig)

  onMsg f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3)
    where
      (a1, a2, a3) = f (cpsmidi)

instance MidiInstr (D -> (Sig, Sig, Sig, Sig)) where
  type MidiInstrOut (D -> (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

  onMsg f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3, toSig (ampmidi 1) * a4)
    where
      (a1, a2, a3, a4) = f (cpsmidi)

instance MidiInstr (D -> SE Sig) where
  type MidiInstrOut (D -> SE Sig) = Sig

  onMsg f msg = do
    a1 <- f ((cpsmidi))
    return $ toSig (ampmidi 1) * a1

instance MidiInstr (D -> SE (Sig, Sig)) where
  type MidiInstrOut (D -> SE (Sig, Sig)) = (Sig, Sig)

  onMsg f msg = do
    (a1, a2) <- f ((cpsmidi))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2)

instance MidiInstr (D -> SE (Sig, Sig, Sig)) where
  type MidiInstrOut (D -> SE (Sig, Sig, Sig)) = (Sig, Sig, Sig)

  onMsg f msg = do
    (a1, a2, a3) <- f ((cpsmidi))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3)

instance MidiInstr (D -> SE (Sig, Sig, Sig, Sig)) where
  type MidiInstrOut (D -> SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)

  onMsg f msg = do
    (a1, a2, a3, a4) <- f ((cpsmidi))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3, toSig (ampmidi 1) * a4)

-------------------------------------------------------------------------------
-- Custom temperament

{- | Converts a value to the midi-instrument with custom temperament.
It's used with the functions 'Csound.Base.midi', 'Csound.Base.midin'.
-}
class (MidiInstr a) => MidiInstrTemp a where
  onMsg' :: Temp -> a -> Msg -> SE (MidiInstrOut a)

-- by (Sig, Sig)

sig2' :: Temp -> Msg -> (Sig, Sig)
sig2' tm msg = (toSig amp, toSig cps)
  where
    (amp, cps) = ampCps' tm msg

instance MidiInstrTemp ((Sig, Sig) -> Sig) where
  onMsg' tm f = return . f . sig2' tm

instance MidiInstrTemp ((Sig, Sig) -> (Sig, Sig)) where
  onMsg' tm f = return . f . sig2' tm

instance MidiInstrTemp ((Sig, Sig) -> (Sig, Sig, Sig)) where
  onMsg' tm f = return . f . sig2' tm

instance MidiInstrTemp ((Sig, Sig) -> (Sig, Sig, Sig, Sig)) where
  onMsg' tm f = return . f . sig2' tm

-- se sig

instance MidiInstrTemp ((Sig, Sig) -> SE Sig) where
  onMsg' tm f = f . sig2' tm

instance MidiInstrTemp ((Sig, Sig) -> SE (Sig, Sig)) where
  onMsg' tm f = f . sig2' tm

instance MidiInstrTemp ((Sig, Sig) -> SE (Sig, Sig, Sig)) where
  onMsg' tm f = f . sig2' tm

instance MidiInstrTemp ((Sig, Sig) -> SE (Sig, Sig, Sig, Sig)) where
  onMsg' tm f = f . sig2' tm

-- by Sig / D

dsig' :: Temp -> Msg -> (D, Sig)
dsig' tm msg = (amp, toSig cps)
  where
    (amp, cps) = ampCps' tm msg

instance MidiInstrTemp ((D, Sig) -> Sig) where
  onMsg' tm f = return . f . dsig' tm

instance MidiInstrTemp ((D, Sig) -> (Sig, Sig)) where
  onMsg' tm f = return . f . dsig' tm

instance MidiInstrTemp ((D, Sig) -> (Sig, Sig, Sig)) where
  onMsg' tm f = return . f . dsig' tm

instance MidiInstrTemp ((D, Sig) -> (Sig, Sig, Sig, Sig)) where
  onMsg' tm f = return . f . dsig' tm

-- se sig

instance MidiInstrTemp ((D, Sig) -> SE Sig) where
  onMsg' tm f = f . dsig' tm

instance MidiInstrTemp ((D, Sig) -> SE (Sig, Sig)) where
  onMsg' tm f = f . dsig' tm

instance MidiInstrTemp ((D, Sig) -> SE (Sig, Sig, Sig)) where
  onMsg' tm f = f . dsig' tm

instance MidiInstrTemp ((D, Sig) -> SE (Sig, Sig, Sig, Sig)) where
  onMsg' tm f = f . dsig' tm

-- by Sig / D

sigd' :: Temp -> Msg -> (Sig, D)
sigd' tm msg = (toSig amp, cps)
  where
    (amp, cps) = ampCps' tm msg

instance MidiInstrTemp ((Sig, D) -> Sig) where
  onMsg' tm f = return . f . sigd' tm

instance MidiInstrTemp ((Sig, D) -> (Sig, Sig)) where
  onMsg' tm f = return . f . sigd' tm

instance MidiInstrTemp ((Sig, D) -> (Sig, Sig, Sig)) where
  onMsg' tm f = return . f . sigd' tm

instance MidiInstrTemp ((Sig, D) -> (Sig, Sig, Sig, Sig)) where
  onMsg' tm f = return . f . sigd' tm

-- se sig

instance MidiInstrTemp ((Sig, D) -> SE Sig) where
  onMsg' tm f = f . sigd' tm

instance MidiInstrTemp ((Sig, D) -> SE (Sig, Sig)) where
  onMsg' tm f = f . sigd' tm

instance MidiInstrTemp ((Sig, D) -> SE (Sig, Sig, Sig)) where
  onMsg' tm f = f . sigd' tm

instance MidiInstrTemp ((Sig, D) -> SE (Sig, Sig, Sig, Sig)) where
  onMsg' tm f = f . sigd' tm

-- d2

d2' :: Temp -> Msg -> (D, D)
d2' tm = ampCps' tm

instance MidiInstrTemp ((D, D) -> Sig) where
  onMsg' tm f = return . f . d2' tm

instance MidiInstrTemp ((D, D) -> (Sig, Sig)) where
  onMsg' tm f = return . f . d2' tm

instance MidiInstrTemp ((D, D) -> (Sig, Sig, Sig)) where
  onMsg' tm f = return . f . d2' tm

instance MidiInstrTemp ((D, D) -> (Sig, Sig, Sig, Sig)) where
  onMsg' tm f = return . f . d2' tm

-- se sig

instance MidiInstrTemp ((D, D) -> SE Sig) where
  onMsg' tm f = f . d2' tm

instance MidiInstrTemp ((D, D) -> SE (Sig, Sig)) where
  onMsg' tm f = f . d2' tm

instance MidiInstrTemp ((D, D) -> SE (Sig, Sig, Sig)) where
  onMsg' tm f = f . d2' tm

instance MidiInstrTemp ((D, D) -> SE (Sig, Sig, Sig, Sig)) where
  onMsg' tm f = f . d2' tm

-- sig

instance MidiInstrTemp (Sig -> Sig) where
  onMsg' tm f msg = return $ toSig (ampmidi 1) * f (toSig (cpsmidi' tm msg))

instance MidiInstrTemp (Sig -> (Sig, Sig)) where
  onMsg' tm f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2)
    where
      (a1, a2) = f (toSig (cpsmidi' tm msg))

instance MidiInstrTemp (Sig -> (Sig, Sig, Sig)) where
  onMsg' tm f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3)
    where
      (a1, a2, a3) = f (toSig (cpsmidi' tm msg))

instance MidiInstrTemp (Sig -> (Sig, Sig, Sig, Sig)) where
  onMsg' tm f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3, toSig (ampmidi 1) * a4)
    where
      (a1, a2, a3, a4) = f (toSig (cpsmidi' tm msg))

instance MidiInstrTemp (Sig -> SE Sig) where
  onMsg' tm f msg = do
    a1 <- f (toSig (cpsmidi' tm msg))
    return $ toSig (ampmidi 1) * a1

instance MidiInstrTemp (Sig -> SE (Sig, Sig)) where
  onMsg' tm f msg = do
    (a1, a2) <- f (toSig (cpsmidi' tm msg))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2)

instance MidiInstrTemp (Sig -> SE (Sig, Sig, Sig)) where
  onMsg' tm f msg = do
    (a1, a2, a3) <- f (toSig (cpsmidi' tm msg))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3)

instance MidiInstrTemp (Sig -> SE (Sig, Sig, Sig, Sig)) where
  onMsg' tm f msg = do
    (a1, a2, a3, a4) <- f (toSig (cpsmidi' tm msg))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3, toSig (ampmidi 1) * a4)

-- d

instance MidiInstrTemp (D -> Sig) where
  onMsg' tm f msg = return $ toSig (ampmidi 1) * f (cpsmidi' tm msg)

instance MidiInstrTemp (D -> (Sig, Sig)) where
  onMsg' tm f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2)
    where
      (a1, a2) = f (cpsmidi' tm msg)

instance MidiInstrTemp (D -> (Sig, Sig, Sig)) where
  onMsg' tm f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3)
    where
      (a1, a2, a3) = f (cpsmidi' tm msg)

instance MidiInstrTemp (D -> (Sig, Sig, Sig, Sig)) where
  onMsg' tm f msg = return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3, toSig (ampmidi 1) * a4)
    where
      (a1, a2, a3, a4) = f (cpsmidi' tm msg)

instance MidiInstrTemp (D -> SE Sig) where
  onMsg' tm f msg = do
    a1 <- f ((cpsmidi' tm msg))
    return $ toSig (ampmidi 1) * a1

instance MidiInstrTemp (D -> SE (Sig, Sig)) where
  onMsg' tm f msg = do
    (a1, a2) <- f ((cpsmidi' tm msg))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2)

instance MidiInstrTemp (D -> SE (Sig, Sig, Sig)) where
  onMsg' tm f msg = do
    (a1, a2, a3) <- f ((cpsmidi' tm msg))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3)

instance MidiInstrTemp (D -> SE (Sig, Sig, Sig, Sig)) where
  onMsg' tm f msg = do
    (a1, a2, a3, a4) <- f ((cpsmidi' tm msg))
    return $ (toSig (ampmidi 1) * a1, toSig (ampmidi 1) * a2, toSig (ampmidi 1) * a3, toSig (ampmidi 1) * a4)
