{-# Language InstanceSigs #-}
-- | With ports we can communicate non-constant values between instruments
module Csound.Typed.Core.Types.SE.Port
  ( Port (..)
  , newPort
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)

import Csound.Dynamic (Rate, E, toInitRate)
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.SE
import Csound.Typed.Core.Types.Tuple
import Csound.Typed.Core.Types.Prim.D
import Csound.Typed.Core.Types.Prim.Val
import Csound.Typed.Core.Types.Prim.Str
import Csound.Typed.Core.State qualified as State

-- https://flossmanual.csound.com/csound-language/local-and-global-variables#the-chn-opcodes-for-global-variables
newtype Port a = Port { unPort :: D }
  deriving (IsPrim, Val, Tuple, Arg)

newPort :: Tuple a => a -> SE (Port a)
newPort initVal = do
  pid <- Port . fromE . pure <$> SE (lift State.getFreshPort)
  writeBy toInitRate pid initVal
  pure pid

instance IsRef Port where
  readRef :: forall a . Tuple a => Port a -> SE a
  readRef (Port pid) = do
    toTuple . pure <$> zipWithM chnget rates names
    where
      names = toPortName pid <$> [1 .. tupleArity @a]
      rates = tupleRates @a

  writeRef = writeBy id

writeBy :: forall a . Tuple a => (Rate -> Rate) -> Port a -> a -> SE ()
writeBy toRate (Port pid) val =
  fromRunSE $ zipWithM_ (\(rate, name) v -> chnset (toRate rate) name v) (zip rates names) <$> (fromTuple val)
  where
    names = toPortName pid <$> [1 .. tupleArity @a]
    rates = tupleRates @a

fromRunSE :: Run (SE ()) -> SE ()
fromRunSE act = SE $ join $ lift (unSE <$> act)

--------------------------------------------------------------
-- utils

toPortName :: D -> Int -> Str
toPortName = undefined

chnset :: Rate -> Str -> E -> SE ()
chnset = undefined

chnget :: Rate -> Str -> SE E
chnget = undefined
