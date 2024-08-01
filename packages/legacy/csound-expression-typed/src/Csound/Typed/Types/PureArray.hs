-- | Pure (read-only) arrays
module Csound.Typed.Types.PureArray (
  PureArr,
  PureArrD,
  newPureArr,
  newPureArrD,
  readPureArr,
  readPureArrD,
) where

import Data.List qualified as List
import Data.Proxy

import Csound.Dynamic (E, IfRate (..))
import Csound.Dynamic qualified as D
import Csound.Typed.GlobalState.GE
import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple

{- | Pure (read-only) array.
We can initialise it and it's a pure value.
After initialisation we can read values with lookupPureArr

This version works on initialisation rate
-}
newtype PureArrD a = PureArrD (GE [E])

{- | Pure (read-only) array.
We can initialise it and it's a pure value.
After initialisation we can read values with lookupPureArr

This version works on control rate
-}
newtype PureArr a = PureArr (GE [E])

-----------------------------------------------------------------------------
-- initialization

-- | Inits pure array that can be read at control rate
newPureArr :: (Tuple a) => [a] -> PureArr a
newPureArr = PureArr . newPureArrBy IfKr

-- | Inits pure array that can be read at initialisation rate
newPureArrD :: (Arg a) => [a] -> PureArrD a
newPureArrD = PureArrD . newPureArrBy IfKr

newPureArrBy :: forall a. (Tuple a) => IfRate -> [a] -> GE [E]
newPureArrBy procRate initVals = do
  initPrimVals <- List.transpose <$> mapM fromTuple initVals
  pure $ zipWith (\outRate initPrims -> D.initPureArr outRate procRate initPrims) outRates initPrimVals
  where
    outRates = tupleRates (Proxy @a)

-----------------------------------------------------------------------------
-- read values

readPureArr :: (Tuple a) => PureArr a -> Sig -> a
readPureArr (PureArr vals) index =
  readPureArrBy IfKr vals (toGE index)

readPureArrD :: (Arg a) => PureArrD a -> D -> a
readPureArrD (PureArrD vals) index =
  readPureArrBy IfIr vals (toGE index)

readPureArrBy :: forall a. (Tuple a) => IfRate -> GE [E] -> GE E -> a
readPureArrBy procRate vals index = toTuple $ do
  indexE <- index
  valsE <- vals
  pure $ zipWith (\outRate arr -> D.readPureArr outRate procRate arr (D.setRate indexRate indexE)) outRates valsE
  where
    indexRate = D.fromIfRate procRate
    outRates = tupleRates (Proxy @a)
