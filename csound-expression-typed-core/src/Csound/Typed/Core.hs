-- | Csound core
module Csound.Typed.Core
  ( -- * Types
    -- ** Primitive types
    D, Sig, Str, Spec
    -- *** Booleans
  , BoolD, BoolSig
  , equals, notEquals, less, lessEquals, greater, greaterEquals
  , notB, (&&*), (||*)
  , ifB
    -- *** Gens
  , module Csound.Typed.Core.Types.Gen

    -- ** Tuples
  , Tuple, Arg, Sigs, makeTupleMethods
  , Sig2, Sig3, Sig4, Sig5, Sig6, Sig7, Sig8
  , D2, D3, D4, D5, D6

    -- ** Arrays

    -- *** Mutable arrays
  , module Csound.Typed.Core.Types.Array

    -- *** Pure arrays
  , module Csound.Typed.Core.Types.PureArray

    -- ** Rate conversions
  , K (..), ar, kr, ir

    -- ** Type conversions
  , sig, int

    -- * Side effects
  , SE
  , renderSE
  , setTotalDur
  , global

    -- ** Writing / reading signals from audio card
  , writeOuts, readIns

    -- ** Imperative if statements
  , when1, whenD1, whens, whenDs
  , untilDo, untilDoD
  , whileDo, whileDoD

    -- ** Mutables
  , IsRef (..)
  , modifyRef
  , Ref, newRef, sensorRef
  , Port, newPort

    -- * Instruments
  , InstrRef
  , MixMode (..)
  , newProc
  , newNamedProc
  , newInstr
  , newEff
  , Note (..)
  , play

  -- * Constants
  , idur
  , getSampleRate
  , getControlRate
  , getBlockSize
  , getZeroDbfs

  -- * Utils
  , fromMono, toMono
  , ceil', frac', floor', int', round', quot', rem', mod', div'

    -- * Options
  , module Csound.Typed.Core.State.Options

    -- * Render
  , module Csound.Typed.Core.Types.SE.Render

    -- * Essential opcodes
  , module Csound.Typed.Core.Opcodes
  ) where

import Data.Boolean
import Csound.Typed.Core.Types
import Csound.Typed.Core.Opcodes
import Csound.Typed.Core.State.Options
import Csound.Typed.Core.Types.Gen
import Csound.Typed.Core.Types.Array
import Csound.Typed.Core.Types.PureArray
import Csound.Typed.Core.Types.SE.Render
