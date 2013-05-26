{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Csound.Exp.EventList(
    CsdSco(..),
    CsdEvent, csdEventStart, csdEventDur, csdEventContent,
    CsdEventList(..), rescaleCsdEventList, rescaleCsdEvent
) where

import Data.Traversable
import Data.Foldable

type CsdEvent a = (Double, Double, a)

csdEventStart   :: CsdEvent a -> Double
csdEventDur     :: CsdEvent a -> Double
csdEventContent :: CsdEvent a -> a

csdEventStart   (a, _, _) = a
csdEventDur     (_, a, _) = a
csdEventContent (_, _, a) = a

class Traversable f => CsdSco f where    
    toCsdEventList :: f a -> CsdEventList a
    singleCsdEvent :: a -> f a

data CsdEventList a = CsdEventList
    { csdEventListDur   :: Double
    , csdEventListNotes :: [CsdEvent a] 
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance CsdSco CsdEventList where
    toCsdEventList = id
    singleCsdEvent a = CsdEventList 1 [(0, 1, a)]

rescaleCsdEventList :: Double -> CsdEventList a -> CsdEventList a
rescaleCsdEventList k (CsdEventList totalDur events) = 
    CsdEventList (k * totalDur) (fmap (rescaleCsdEvent k) events)

rescaleCsdEvent :: Double -> CsdEvent a -> CsdEvent a
rescaleCsdEvent k (start, dur, a) = (k * start, k * dur, a)

