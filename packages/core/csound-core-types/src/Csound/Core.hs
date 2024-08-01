-- | Csound core
module Csound.Core (
  -- * Types

  -- ** Primitive types
  D,
  Sig,
  Str,
  Spec,

  -- *** Booleans
  BoolD,
  BoolSig,
  equals,
  notEquals,
  less,
  lessEquals,
  greater,
  greaterEquals,
  notB,
  (&&*),
  (||*),
  ifB,

  -- *** Gens
  module Csound.Core.Types.Gen,

  -- ** Tuples
  FromTuple (..),
  Tuple (..),
  Arg,
  Sigs,
  Sig2,
  Sig3,
  Sig4,
  Sig5,
  Sig6,
  Sig7,
  Sig8,
  D2,
  D3,
  D4,
  D5,
  D6,

  -- ** Arrays

  -- *** Mutable arrays
  module Csound.Core.Types.Array,

  -- *** Pure arrays
  module Csound.Core.Types.PureArray,

  -- ** Rate conversions
  K (..),
  ar,
  kr,
  ir,

  -- ** Type conversions
  toSig,
  toD,
  int,
  double,
  float,

  -- ** Signal space
  SigSpace (..),
  BindSig (..),
  mul,
  mul',
  on,
  uon,
  At (..),
  MixAt (..),
  cfd,
  genCfds,
  cfd4,
  cfds,
  mean,

  -- * Stereo sig-space
  SigSpace2 (..),
  BindSig2 (..),
  mul2,
  mul2',

  -- * Side effects
  SE,
  renderSE,
  setTotalDur,
  global,
  setOption,
  setDefaultOption,

  -- ** Writing / reading signals from audio card
  writeOuts,
  readIns,

  -- ** Imperative if statements
  when1,
  whens,
  whileDo,
  untilDo,
  doRepeat,

  -- ** Mutables
  IsRef (..),
  modifyRef,
  modifyInitRef,
  Ref,
  newRef,
  newLocalRef,
  newCtrlRef,
  newLocalCtrlRef,
  newInitRef,
  newLocalInitRef,
  getInstrRefId,
  getInstrRefIdNum,
  instrRefFromNum,
  Port,
  newPort,

  -- * Instruments
  InstrRef,
  MixMode (..),
  newProc,
  newNamedProc,
  newInstr,
  newEff,
  Note (..),
  play,
  setFraction,
  negateInstrRef,

  -- * Constants
  idur,
  iself,
  getSampleRate,
  getControlRate,
  getBlockSize,
  getZeroDbfs,

  -- * Utils
  fromMono,
  toMono,
  ceil',
  frac',
  floor',
  int',
  round',
  quot',
  rem',
  mod',
  div',
  withInits,
  withDs,
  withD,
  withSig,
  withSigs,
  withTab,
  withTabs,

  -- * Render
  module Csound.Core.Render,
  module Csound.Core.Render.Options,

  -- * Essential opcodes
  module Csound.Core.Opcodes,

  -- * Reexports
  module Data.String,
) where

import Csound.Core.Opcodes
import Csound.Core.Render
import Csound.Core.Render.Options
import Csound.Core.Types
import Csound.Core.Types.Array
import Csound.Core.Types.Gen
import Csound.Core.Types.PureArray
import Data.Boolean
import Data.String
