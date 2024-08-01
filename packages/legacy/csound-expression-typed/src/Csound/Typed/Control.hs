{-# LANGUAGE ScopedTypeVariables #-}

module Csound.Typed.Control (
  -- * SE
  module Csound.Typed.GlobalState.SE,

  -- ** SE reference
  module Csound.Typed.Control.Ref,

  -- * Global settings
  instr0,
  getIns,
  setDur,

  -- * Misc
  freshId,

  -- * Score
  module Csound.Typed.Control.Mix,

  -- * Midi
  module Csound.Typed.Control.Midi,

  -- * Named instruments (API)
  module Csound.Typed.Control.Api,

  -- * OSC
  module Csound.Typed.Control.Osc,

  -- * Channel
  module Csound.Typed.Control.Channel,

  -- * Sf2
  module Csound.Typed.Control.Sf2,

  -- * Events
  module Csound.Typed.Control.Evt,

  -- * Band-limited oscillators
  module Csound.Typed.Control.Vco,

  -- * Imperative instruments
  module Csound.Typed.Control.InstrRef,

  -- * Array folding and traversals
  module Csound.Typed.Control.ArrayTraverse,

  -- * Reads global config arguments from command line
  module Csound.Typed.Control.MacrosArgs,
) where

import Csound.Typed.Control.Ref
import Csound.Typed.GlobalState.SE
import Data.Proxy

import Csound.Typed.Control.Api
import Csound.Typed.Control.ArrayTraverse
import Csound.Typed.Control.Channel
import Csound.Typed.Control.Evt
import Csound.Typed.Control.InstrRef
import Csound.Typed.Control.MacrosArgs
import Csound.Typed.Control.Midi
import Csound.Typed.Control.Mix
import Csound.Typed.Control.Osc
import Csound.Typed.Control.Sf2
import Csound.Typed.Control.Vco

import Csound.Typed.GlobalState
import Csound.Typed.Types

instr0 :: forall a. (Tuple a) => SE a -> SE a
instr0 a = return $ toTuple $ saveIns0 ins0Arity (tupleRates (Proxy :: Proxy a)) ins0Exp
  where
    ins0Exp = execGEinSE $ fmap fromTuple a
    ins0Arity = tupleArity (Proxy :: Proxy a)

getIns :: forall a. (Sigs a) => SE a
getIns = fmap toTuple $ fromDep $ getIn (tupleArity (Proxy :: Proxy a))

{- | Sets the global duration of the file or output signal to the given value.
It should be used only once! The proper place is in the top-most
expression before sending to @dac@ or @writeWav@.
-}
setDur :: (Sigs a) => D -> a -> a
setDur mdt as = toTuple $ do
  dt <- toGE mdt
  vals <- fromTuple as
  setDurationForce dt
  return vals

-- | Gets new id.
freshId :: SE D
freshId = SE $ fmap fromE freeChn
