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
import Csound.Exp.Instr
import qualified Csound.Render.IndexMap as DM

data Event a where
    -- event sources
    Trigger     :: BoolSig -> Event ()
    TriggerK    :: BoolSig -> a -> Event a
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
                    => (Snap a -> b -> c) -> a -> Event b -> Event c
    -- operation
    Empty       :: Event a
    Merge       :: Event a -> Event a -> Event a
    
    -- switch ???
    Switch      :: Arg a => (a -> Event b) -> Event a -> Event b


instance Monoid (Event a) where
    mempty  = Empty
    mappend a b = case (a, b) of
        (Empty, a) -> a
        (a, Empty) -> a
        (a, b) -> Merge a b

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

keyboard :: Event D
keyboard = Keyboard

mouseLeft :: Event (D, D)
mouseLeft = MouseLeft

mouseRight :: Event (D, D)
mouseRight = MouseRight

button :: Event ()
button = Button
    
-- transform

snapshot :: (CsdTuple a, CsdTuple b, CsdTuple c) => (Snap a -> b -> c) -> a -> Event b -> Event c
snapshot f x y = case y of
    Empty -> Empty    
    _ -> Snapshot f x y

mapEvent :: (CsdTuple a, CsdTuple b) => (a -> b) -> Event a -> Event b
mapEvent f x = case x of
    Empty -> Empty
    Trigger a -> TriggerK a (f ())
    TriggerK a b -> TriggerK a (f b)
    Map g a -> Map (f . g) a
    Accum s g a -> Accum s (\s x -> let (y, s') = g s x in (f y, s)) a
    Snapshot g a b -> Snapshot (\a b -> f (g a b)) a b

filterEvent :: (CsdTuple a) => (a -> BoolD) -> Event a -> Event a
filterEvent p x = case x of
    Empty -> Empty
    _ -> Filter p x

accumEvent :: (CsdTuple a, CsdTuple b, CsdTuple s) 
    => s -> (s -> a -> (b, s)) -> Event a -> Event b    
accumEvent s0 f x = case x of
    Empty -> Empty
    Map g a -> Accum s0 (\s a -> f s (g a)) a    
    _ -> Accum s0 f x

--------------------------------------------------
--

schedule :: (Arg a, Out b) => (a -> b) -> Event (D, a) -> SE (NoSE b)
schedule instr trigger = do    
    name <- saveInstr instr
    saveTrigger trigger name

toggle :: (Arg a, Out b) => (a -> b) -> Event a -> Event c -> SE (NoSE b)
toggle = undefined

--------------
--

saveTrigger :: CsdTuple b => Event (D, a) -> DM.InstrName -> SE b
saveTrigger = undefined

