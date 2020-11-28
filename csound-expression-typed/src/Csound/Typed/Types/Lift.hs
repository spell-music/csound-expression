{-# Language FlexibleInstances #-}
module Csound.Typed.Types.Lift(
    GE, E,
    -- * Lifters
    -- ** Pure single
    PureSingle, pureSingle,

    -- ** Dirty single
    DirtySingle, dirtySingle,

    -- ** Procedure
    Procedure, procedure,

    -- ** Pure multi 
    PureMulti, Pm, fromPm, pureMulti,

    -- ** Dirty multi
    DirtyMulti, Dm, fromDm, dirtyMulti
        
) where

import Control.Applicative

import Csound.Dynamic
import Csound.Typed.Types.Prim
import Csound.Typed.Types.Tuple
import Csound.Typed.GlobalState
    
pureSingle :: PureSingle a => ([E] -> E) -> a
pureSingle = pureSingleGE . return
    
dirtySingle :: DirtySingle a => ([E] -> Dep E) -> a
dirtySingle = dirtySingleGE . return
    
procedure :: Procedure a => ([E] -> Dep ()) -> a
procedure = procedureGE . return

newtype Pm = Pm (GE (MultiOut [E]))

pureMulti :: PureMulti a => ([E] -> MultiOut [E]) -> a
pureMulti = pureMultiGE . return

newtype Dm = Dm (GE (MultiOut (Dep [E])))

dirtyMulti :: DirtyMulti a => ([E] -> MultiOut (Dep [E])) -> a
dirtyMulti = dirtyMultiGE . return

class PureSingle a where
    pureSingleGE :: GE ([E] -> E) -> a

class DirtySingle a where
    dirtySingleGE :: GE ([E] -> Dep E) -> a

class Procedure a where
    procedureGE :: GE ([E] -> Dep ()) -> a
    
class PureMulti a where
    pureMultiGE :: GE ([E] -> MultiOut [E]) -> a
    
class DirtyMulti a where
    dirtyMultiGE :: GE ([E] -> MultiOut (Dep [E])) -> a

-- multi out helpers

fromPm :: Tuple a => Pm -> a
fromPm (Pm a) = res
    where res = toTuple $ fmap ( $ tupleArity res) a

fromDm :: Tuple a => Dm -> SE a
fromDm (Dm a) = res
    where 
        res = fmap toTuple $ fromDep $ hideGEinDep $ fmap ( $ (tupleArity $ proxy res)) a

        proxy :: SE a -> a
        proxy = const undefined

-- pure single

instance PureSingle (GE E) where
    pureSingleGE = fmap ($ [])

instance PureSingle b => PureSingle (GE E -> b) where
    pureSingleGE mf = \ma -> pureSingleGE $ (\f a as -> f (a:as)) <$> mf <*> ma

instance PureSingle b => PureSingle (GE [E] -> b) where
    pureSingleGE mf = \mas -> pureSingleGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mas
    
ps0 :: (Val a) => GE ([E] -> E) -> a
ps0 = fromGE . pureSingleGE

ps1 :: (Val a, PureSingle b) => GE ([E] -> E) -> (a -> b)
ps1 f = pureSingleGE f . toGE

pss :: (Val a, PureSingle b) => GE ([E] -> E) -> ([a] -> b)
pss f = pureSingleGE f . mapM toGE

instance PureSingle Sig   where   pureSingleGE = ps0
instance PureSingle D     where   pureSingleGE = ps0
instance PureSingle Str   where   pureSingleGE = ps0
instance PureSingle Tab   where   pureSingleGE = ps0
instance PureSingle Spec  where   pureSingleGE = ps0
instance PureSingle Wspec where   pureSingleGE = ps0

instance (PureSingle b) => PureSingle (Sig -> b)    where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (D -> b)      where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (Str -> b)    where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (Tab -> b)    where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (Spec -> b)   where   pureSingleGE = ps1
instance (PureSingle b) => PureSingle (Wspec -> b)  where   pureSingleGE = ps1

instance (PureSingle b) => PureSingle ([Sig] -> b)  where   pureSingleGE = pss
instance (PureSingle b) => PureSingle ([D] -> b)    where   pureSingleGE = pss

instance (PureSingle b) => PureSingle (Msg -> b)    where   pureSingleGE f = const $ pureSingleGE f

-- dirty single

instance DirtySingle (SE (GE E)) where
    dirtySingleGE = fromDep . hideGEinDep . fmap ($ [])
    
instance DirtySingle b => DirtySingle (GE E -> b) where
    dirtySingleGE mf = \ma -> dirtySingleGE $ (\f a as -> f (a:as)) <$> mf <*> ma
    
instance DirtySingle b => DirtySingle (GE [E] -> b) where
    dirtySingleGE mf = \mas -> dirtySingleGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mas

ds0 :: (Val a) => GE ([E] -> Dep E) -> SE a
ds0 = fmap fromGE . dirtySingleGE

ds1 :: (Val a, DirtySingle b) => GE ([E] -> Dep E) -> (a -> b)
ds1 f = dirtySingleGE f . toGE

dss :: (Val a, DirtySingle b) => GE ([E] -> Dep E) -> ([a] -> b)
dss f = dirtySingleGE f . mapM toGE

instance DirtySingle (SE Sig)   where   dirtySingleGE = ds0
instance DirtySingle (SE D)     where   dirtySingleGE = ds0
instance DirtySingle (SE Str)   where   dirtySingleGE = ds0
instance DirtySingle (SE Tab)   where   dirtySingleGE = ds0
instance DirtySingle (SE Spec)  where   dirtySingleGE = ds0
instance DirtySingle (SE Wspec) where   dirtySingleGE = ds0

instance (DirtySingle b) => DirtySingle (Sig -> b)    where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (D -> b)      where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (Str -> b)    where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (Tab -> b)    where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (Spec -> b)   where   dirtySingleGE = ds1
instance (DirtySingle b) => DirtySingle (Wspec -> b)  where   dirtySingleGE = ds1

instance (DirtySingle b) => DirtySingle ([Sig] -> b)  where   dirtySingleGE = dss
instance (DirtySingle b) => DirtySingle ([D] -> b)    where   dirtySingleGE = dss

instance (DirtySingle b) => DirtySingle (Msg -> b)    where   dirtySingleGE f = const $ dirtySingleGE f

-- procedure

instance Procedure (SE ()) where
    procedureGE = fromDep_ . hideGEinDep . fmap ($ [])

instance Procedure b => Procedure (GE E -> b) where
    procedureGE mf = \ma -> procedureGE $ (\f a as -> f (a:as)) <$> mf <*> ma
    
instance Procedure b => Procedure (GE [E] -> b) where
    procedureGE mf = \mas -> procedureGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mas
    
pr1 :: (Val a, Procedure b) => GE ([E] -> Dep ()) -> a -> b
pr1 f = procedureGE f . toGE

prs :: (Val a, Procedure b) => GE ([E] -> Dep ()) -> ([a] -> b)
prs f = procedureGE f . mapM toGE

instance (Procedure b) => Procedure (Sig -> b)    where   procedureGE = pr1
instance (Procedure b) => Procedure (D -> b)      where   procedureGE = pr1
instance (Procedure b) => Procedure (Str -> b)    where   procedureGE = pr1
instance (Procedure b) => Procedure (Tab -> b)    where   procedureGE = pr1
instance (Procedure b) => Procedure (Spec -> b)   where   procedureGE = pr1
instance (Procedure b) => Procedure (Wspec -> b)  where   procedureGE = pr1

instance (Procedure b) => Procedure ([Sig] -> b)  where   procedureGE = prs
instance (Procedure b) => Procedure ([D] -> b)    where   procedureGE = prs

instance (Procedure b) => Procedure (Msg -> b)    where   procedureGE f = const $ procedureGE f

-- pure multi

instance PureMulti Pm where
    pureMultiGE = Pm . fmap ($ [])

instance PureMulti b => PureMulti (GE E -> b) where
    pureMultiGE mf = \ma -> pureMultiGE $ (\f a as -> f (a:as)) <$> mf <*> ma
    
instance PureMulti b => PureMulti (GE [E] -> b) where
    pureMultiGE mf = \mas -> pureMultiGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mas

pm1 :: (Val a, PureMulti b) => GE ([E] -> MultiOut [E]) -> (a -> b)
pm1 f = pureMultiGE f . toGE

pms :: (Val a, PureMulti b) => GE ([E] -> MultiOut [E]) -> ([a] -> b)
pms f = pureMultiGE f . mapM toGE 

instance (PureMulti b) => PureMulti (Sig -> b)    where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (D -> b)      where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (Str -> b)    where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (Tab -> b)    where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (Spec -> b)   where   pureMultiGE = pm1
instance (PureMulti b) => PureMulti (Wspec -> b)  where   pureMultiGE = pm1

instance (PureMulti b) => PureMulti ([Sig] -> b)  where   pureMultiGE = pms
instance (PureMulti b) => PureMulti ([D] -> b)    where   pureMultiGE = pms

instance (PureMulti b) => PureMulti (Msg -> b)    where   pureMultiGE f = const $ pureMultiGE f

-- dirty multi

instance DirtyMulti Dm where
    dirtyMultiGE = Dm . fmap ($ [])

instance DirtyMulti b => DirtyMulti (GE E -> b) where
    dirtyMultiGE mf = \ma -> dirtyMultiGE $ (\f a as -> f (a:as)) <$> mf <*> ma

instance DirtyMulti b => DirtyMulti (GE [E] -> b) where
    dirtyMultiGE mf = \mas -> dirtyMultiGE $ (\f as bs -> f (as ++ bs)) <$> mf <*> mas

dm1 :: (Val a, DirtyMulti b) => GE ([E] -> MultiOut (Dep [E])) -> (a -> b)
dm1 f = dirtyMultiGE f . toGE

dms :: (Val a, DirtyMulti b) => GE ([E] -> MultiOut (Dep [E])) -> ([a] -> b)
dms f = dirtyMultiGE f . mapM toGE

instance (DirtyMulti b) => DirtyMulti (Sig -> b)    where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (D -> b)      where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (Str -> b)    where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (Tab -> b)    where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (Spec -> b)   where   dirtyMultiGE = dm1
instance (DirtyMulti b) => DirtyMulti (Wspec -> b)   where   dirtyMultiGE = dm1

instance (DirtyMulti b) => DirtyMulti ([Sig] -> b)  where   dirtyMultiGE = dms
instance (DirtyMulti b) => DirtyMulti ([D] -> b)    where   dirtyMultiGE = dms

instance (DirtyMulti b) => DirtyMulti (Msg -> b)    where   dirtyMultiGE f = const $ dirtyMultiGE f
