-- | Named channels.
--
-- With named channels we can read and 
-- write values to the variables with dynamic names.
-- We can specify the variable with string (Str).
--
-- Csound has an C api wich is ported to many languages.
-- With named channels we can interact with csound
-- that runns a program. We can read and write to named channels
-- from another program.
module Csound.Typed.Control.Channel(
    -- * Getters
    chnGetD, chnGetSig, chnGetCtrl, chnGetStr,

    -- * Setters
    chnSetD, chnSetSig, chnSetCtrl, chnSetStr
) where

import Control.Monad.Trans.Class

import Csound.Dynamic(Rate(..))
import Csound.Typed.Types
import Csound.Typed.GlobalState

-- getters

-- | Reads a value of type double.
chnGetD :: Str -> SE D
chnGetD = get Ir

-- | Reads a control signal. 
-- The control signals are updated at 
-- the lower rate. 
chnGetCtrl :: Str -> SE Sig
chnGetCtrl = get Kr

-- | Reads an audio signal.
chnGetSig :: Str -> SE Sig
chnGetSig = get Ar

-- | Reads a string.
chnGetStr :: Str -> SE Str
chnGetStr = get Sr

-- setters


-- | Writes a value of type double.
chnSetD :: D -> Str -> SE ()
chnSetD = set Ir

-- | Writes an audio signal.
chnSetSig :: Sig -> Str -> SE ()
chnSetSig = set Ar

-- | Writes a control signal. 
-- The control signals are updated at 
-- the lower rate. 
chnSetCtrl :: Sig -> Str -> SE ()
chnSetCtrl = set Kr

-- | Writes a string.
chnSetStr :: Str -> Str -> SE ()
chnSetStr = set Sr

------------------------------------------------------

get :: Val a => Rate -> Str -> SE a
get rate chn = fmap fromGE $ fromDep $ (chnGet rate) =<< (lift $ unStr chn)

set :: Val a => Rate -> a -> Str -> SE ()
set rate val chn = fromDep_ $ do
    v <- lift $ toGE val
    c <- lift $ unStr chn
    chnSet rate v c

