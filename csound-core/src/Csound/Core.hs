-- | Csound core
module Csound.Core
  ( -- * Types
    -- ** Primitive types
    D, Sig, Str, Spec
    -- *** Booleans
  , BoolD, BoolSig
  , equals, notEquals, less, lessEquals, greater, greaterEquals
  , notB, (&&*), (||*)
  , ifB
    -- *** Gens
  , module Csound.Core.Types.Gen

    -- ** Tuples
  , Tuple, Arg, Sigs, makeTupleMethods
  , Sig2, Sig3, Sig4, Sig5, Sig6, Sig7, Sig8
  , D2, D3, D4, D5, D6

    -- ** Arrays

    -- *** Mutable arrays
  , module Csound.Core.Types.Array

    -- *** Pure arrays
  , module Csound.Core.Types.PureArray

    -- ** Rate conversions
  , K (..), ar, kr, ir

    -- ** Type conversions
  , sig, toD, int

   -- ** Signal space
  , SigSpace(..), BindSig(..), mul, mul', on, uon, At(..), MixAt(..)
  , cfd, genCfds, cfd4, cfds
  , mean

    -- * Stereo sig-space
  , SigSpace2(..), BindSig2(..), mul2, mul2'

    -- * Side effects
  , SE
  , renderSE
  , setTotalDur
  , global
  , setOption
  , setDefaultOption

    -- ** Writing / reading signals from audio card
  , writeOuts, readIns

    -- ** Imperative if statements
  , when1, whens, whileDo, untilDo, doRepeat

    -- ** Mutables
  , IsRef (..)
  , modifyRef
  , Ref, newRef, newLocalRef
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
  , setFraction
  , negateInstrRef

  -- * Constants
  , idur
  , iself
  , getSampleRate
  , getControlRate
  , getBlockSize
  , getZeroDbfs

  -- * Utils
  , fromMono, toMono
  , ceil', frac', floor', int', round', quot', rem', mod', div'
  , withInits
  , withDs, withD, withSig, withSigs, withTab, withTabs

    -- * Options
  , module Csound.Core.State.Options

    -- * Render
  , module Csound.Core.Types.SE.Render

    -- * Essential opcodes
  , module Csound.Core.Opcodes

    -- * Reexports
  , module Data.String
  ) where


import Data.String
import Data.Boolean
import Csound.Core.Types
import Csound.Core.Opcodes
import Csound.Core.State.Options
import Csound.Core.Types.Gen
import Csound.Core.Types.Array
import Csound.Core.Types.PureArray
import Csound.Core.Types.SE.Render
