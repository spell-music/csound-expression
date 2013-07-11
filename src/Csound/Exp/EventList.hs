{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Csound.Exp.EventList(
    CsdSco(..), 
    CsdEvent, csdEventStart, csdEventDur, csdEventContent,
    CsdEventList(..), delayCsdEventList, rescaleCsdEventList
) where

import Data.Traversable
import Data.Foldable

-- | Csound note. It's a triple of
--
-- > (startTime, duration, parameters)
type CsdEvent a = (Double, Double, a)

csdEventStart   :: CsdEvent a -> Double
csdEventDur     :: CsdEvent a -> Double
csdEventContent :: CsdEvent a -> a

csdEventStart   (a, _, _) = a
csdEventDur     (_, a, _) = a
csdEventContent (_, _, a) = a

csdEventTotalDur :: CsdEvent a -> Double
csdEventTotalDur (start, dur, _) = start + dur

-- | Class that represents Csound scores. All functions that use score are defined
-- in terms of this class. If you want to use your own score representation, just define
-- two methods of the class.
class CsdSco f where    
    -- | Converts a given score representation to the canonical one.
    toCsdEventList :: f a -> CsdEventList a
    -- | Constructs a scores that contains only one event.
    singleCsdEvent ::  CsdEvent a -> f a

-- | 'Csound.Base.CsdEventList' is a canonical representation of the Csound scores.
-- Scores is a list of events and we should know the total duration of the scores.
-- It's not meant to be used directly. We can use a better alternative. More convenient
-- type that belongs to 'Csound.Base.CsdSco' type class.
data CsdEventList a = CsdEventList
    { csdEventListDur   :: Double
    , csdEventListNotes :: [CsdEvent a] 
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance CsdSco CsdEventList where
    toCsdEventList = id
    singleCsdEvent evt = CsdEventList (csdEventTotalDur evt) [evt]

delayCsdEventList :: Double -> CsdEventList a -> CsdEventList a
delayCsdEventList k (CsdEventList totalDur events) = 
    CsdEventList (k + totalDur) (fmap (delayCsdEvent k) events)

delayCsdEvent :: Double -> CsdEvent a -> CsdEvent a 
delayCsdEvent k (start, dur, a) = (k + start, dur, a)

rescaleCsdEventList :: Double -> CsdEventList a -> CsdEventList a
rescaleCsdEventList k (CsdEventList totalDur events) = 
    CsdEventList (k * totalDur) (fmap (rescaleCsdEvent k) events)

rescaleCsdEvent :: Double -> CsdEvent a -> CsdEvent a
rescaleCsdEvent k (start, dur, a) = (k * start, k * dur, a)

