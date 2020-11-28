module Csound.Typed.GlobalState.Cache(
    Cache(..), HashKey,    
    -- * Mix
    -- ** Functions
    CacheMix, MixKey(..),
    saveMixKey, getMixKey,
    -- ** Procedures
    CacheMixProc, 
    saveMixProcKey, getMixProcKey,
    -- * Evt
    -- ** Functions
    CacheEvt, EvtKey(..),
    saveEvtKey, getEvtKey,
    -- ** Procedures
    CacheEvtProc, 
    saveEvtProcKey, getEvtProcKey
) where

import qualified Data.Map as M
import Data.Default

import Csound.Dynamic

data Cache m = Cache 
    { cacheMix      :: CacheMix
    , cacheMixProc  :: CacheMixProc m
    , cacheEvt      :: CacheEvt
    , cacheEvtProc  :: CacheEvtProc m }

instance Default (Cache m) where
    def = Cache def def def def

type HashKey = Int

type GetKey  m a b = a -> Cache m -> Maybe b
type SaveKey m a b = a -> b -> Cache m -> Cache m

getKeyMap :: (Ord key) => (Cache m -> M.Map key val) -> GetKey m key val
getKeyMap f key x = M.lookup key $ f x

saveKeyMap :: (Ord key) => (Cache m -> M.Map key val) -> (M.Map key val -> Cache m -> Cache m) -> SaveKey m key val
saveKeyMap getter setter key val cache = setter (M.insert key val $ getter cache) cache

----------------------------------------------------------
-- Mix

-- Mix functions

newtype MixKey = MixKey HashKey
    deriving (Eq, Ord)

type    MixVal = InstrId

type CacheMix = M.Map MixKey MixVal

getMixKey :: GetKey m MixKey MixVal
getMixKey = getKeyMap cacheMix

saveMixKey :: SaveKey m MixKey MixVal
saveMixKey = saveKeyMap cacheMix (\a x -> x { cacheMix = a })

-- Mix procedures

type CacheMixProc m = M.Map MixKey (DepT m ())

getMixProcKey :: GetKey m MixKey (DepT m ())
getMixProcKey = getKeyMap cacheMixProc

saveMixProcKey :: SaveKey m MixKey (DepT m ())
saveMixProcKey = saveKeyMap cacheMixProc (\a x -> x { cacheMixProc = a })

----------------------------------------------------------
-- Evt

-- Evt functions

data EvtKey = EvtKey HashKey HashKey
    deriving (Eq, Ord)

type    EvtVal = InstrId

type CacheEvt = M.Map EvtKey EvtVal

getEvtKey :: GetKey m EvtKey EvtVal
getEvtKey = getKeyMap cacheEvt

saveEvtKey :: SaveKey m EvtKey EvtVal
saveEvtKey = saveKeyMap cacheEvt (\a x -> x { cacheEvt = a })

-- Evt procedures

type CacheEvtProc m = M.Map EvtKey (DepT m ())

getEvtProcKey :: GetKey m EvtKey (DepT m ())
getEvtProcKey = getKeyMap cacheEvtProc

saveEvtProcKey :: SaveKey m EvtKey (DepT m ())
saveEvtProcKey = saveKeyMap cacheEvtProc (\a x -> x { cacheEvtProc = a })

