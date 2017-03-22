-- | Basic types and functions.
--
-- This module re-exports everything.
--
-- WARNING (for Csound users): the maximum amplitude is 1.0. There is no way to alter it. 
-- Don't define your amplitudes with 9000 or 11000. But the good news are: all signals
-- are clipped by 1 so that you can not damage your ears and your speakers by a little typo.
module Csound.Base(
    module Csound.Types, 
    module Csound.Control,
    module Csound.IO,
    module Csound.Air,
    module Csound.Tab,
    module Csound.Tuning,
    module Csound.Options,
    module Csound.SigSpace,

    -- * Standard classes
    module Data.Boolean,
    module Data.Default,
    module Data.Monoid,
    module Control.Applicative,

    module Temporal.Media,
    module Temporal.Class,

    -- * Opcodes
    module Csound.Typed.Opcode
) where

import Csound.Air 
import Csound.Tab
import Csound.Tuning
import Csound.Types
import Csound.Control
import Csound.IO
import Csound.SigSpace
import Csound.Options

import Temporal.Media
import Temporal.Class

import Data.Boolean
import Data.Default
import Data.Monoid
import Control.Applicative hiding ((<*))
    
import Csound.Typed.Opcode hiding (
    button, display, space, lfo, initc7, ctrl7,
    oscInit, oscListen, oscSend, 
    lpshold, loopseg, loopxseg,
    partikkel, syncgrain, granule, sndwarp, sndwarpst, fof2,
    line, delay,
    metro, dust,
    duserrnd, cuserrnd, urd,
    jackoInfo, jackoFreewheel, turnoff2)
