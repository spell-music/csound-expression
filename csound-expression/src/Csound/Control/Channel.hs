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
module Csound.Control.Channel(
    -- * Getters
    chnGetD, chnGetSig, chnGetCtrl, chnGetStr,

    -- * Setters
    chnSetD, chnSetSig, chnSetCtrl, chnSetStr
) where

import Csound.Typed

