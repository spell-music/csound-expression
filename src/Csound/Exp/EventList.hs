{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Csound.Exp.EventList(
    CsdSco(..),
    CsdEvent, csdEventStart, csdEventDur, csdEventContent,
    CsdEventList(..), delayCsdEventList, rescaleCsdEventList
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

class CsdSco f where    
    toCsdEventList :: f a -> CsdEventList a
    singleCsdEvent :: Double -> Double -> a -> f a

data CsdEventList a = CsdEventList
    { csdEventListDur   :: Double
    , csdEventListNotes :: [CsdEvent a] 
    } deriving (Eq, Show, Functor, Foldable, Traversable)

instance CsdSco CsdEventList where
    toCsdEventList = id
    singleCsdEvent start dur a = CsdEventList (start + dur) [(start, dur, a)]

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

