-- | Primitive Csound types
module Csound.Typed.Core.Types.Prim
  ( InstrId (..)
  , SigOrD
  , module X
  -- * Converters
  , sig
  -- * Constants
  , idur
  , getSampleRate
  , getControlRate
  , getBlockSize
  , getZeroDbfs
  -- * Utils
  , ceil'
  ) where

import Csound.Typed.Core.State
import Csound.Dynamic (E, Name, Rate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.Types.Prim.Bool as X
import Csound.Typed.Core.Types.Prim.D as X
import Csound.Typed.Core.Types.Prim.Sig as X
import Csound.Typed.Core.Types.Prim.Tab as X
import Csound.Typed.Core.Types.Prim.Val as X
import Csound.Typed.Core.Types.Prim.Str as X

class Val a => SigOrD a where

instance SigOrD Sig
instance SigOrD D

newtype InstrId = InstrId { unInstrId :: Run E }

instance Val InstrId where
  fromE = InstrId
  toE   = unInstrId

-------------------------------------------------------------------------------
-- converters

sig :: D -> Sig
sig (D a) = Sig a

ceil' :: D -> D
ceil' = liftE Dynamic.ceilE

-------------------------------------------------------------------------------
-- constants

-- | Querries a total duration of the note. It's equivallent to Csound's @p3@ field.
idur :: D
idur = fromE $ pure $ Dynamic.pn 3

getSampleRate :: D
getSampleRate = readConstant "sr"

getControlRate :: D
getControlRate = readConstant "kr"

getBlockSize :: D
getBlockSize = readConstant "ksmps"

getZeroDbfs :: D
getZeroDbfs =  readConstant "0dbfs"

readConstant :: Val a => Name -> a
readConstant name = fromE $ pure $ Dynamic.readOnlyVar (Dynamic.VarVerbatim Ir name)
