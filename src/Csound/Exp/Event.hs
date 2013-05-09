{-# Language GADTs, TypeFamilies #-}
module Csound.Exp.Event where

import Data.Monoid
import Temporal.Music.Score(Score)

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.Logic
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Exp.Mix
import Csound.Exp.SE


data Event a where
    -- event sources
    Trigger     :: BoolSig -> Event ()
    Keyboard    :: Event D
    MouseLeft   :: Event (D, D)
    MouseRight  :: Event (D, D)
    Button      :: Event ()
    
    -- transform
    Map         :: (CsdTuple a, CsdTuple b) => (a -> b) -> Event a -> Event b
    Filter      :: (CsdTuple a) => (a -> BoolD) -> Event a -> Event a
    Accum       :: (CsdTuple a, CsdTuple b, CsdTuple s)  
                    => s -> (s -> a -> (b, s)) -> Event a -> Event b
    Snapshot    :: (CsdTuple a, CsdTuple b, CsdTuple c) 
                    => (a -> b -> c) -> a -> Event b -> Event c
    -- operation
    Empty       :: Event a
    Merge       :: Event a -> Event a -> Event a


instance Monoid (Event a) where
    mempty  = Empty
    mappend = Merge

-- snap 

type family Snap a :: *

type instance Snap D   = D
type instance Snap Str = Str
type instance Snap Tab = Tab

type instance Snap Sig = D

type instance Snap (a, b) = (Snap a, Snap b)
type instance Snap (a, b, c) = (Snap a, Snap b, Snap c)
type instance Snap (a, b, c, d) = (Snap a, Snap b, Snap c, Snap d)

-----------------------------------------
-- smart constructors

trigger :: BoolSig -> Event ()
trigger = Trigger

snapshot :: (CsdTuple a, CsdTuple b, CsdTuple c) => (a -> b -> c) -> a -> Event b -> Event c
snapshot = Snapshot

keyboard :: Event D
keyboard = Keyboard

mouseLeft :: Event (D, D)
mouseLeft = MouseLeft

mouseRight :: Event (D, D)
mouseRight = MouseRight

button :: Event ()
button = Button
    
-- transform
mapEvent :: (CsdTuple a, CsdTuple b) => (a -> b) -> Event a -> Event b
mapEvent = Map

filterEvent :: (CsdTuple a) => (a -> BoolD) -> Event a -> Event a
filterEvent = Filter

accumEvent :: (CsdTuple a, CsdTuple b, CsdTuple s) 
    => s -> (s -> a -> (b, s)) -> Event a -> Event b    
accumEvent = Accum

--------------------------------------------------
--

schedule :: (Arg a, Out b) => (a -> b) -> Event (D, a) -> SE (NoSE b)
schedule = undefined

toggle :: (Arg a, Out b) => (a -> b) -> Event a -> Event c -> SE (NoSE b)
toggle = undefined

--------------
--

