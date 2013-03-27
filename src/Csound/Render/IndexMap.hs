module Csound.Render.IndexMap(
    IndexMap, empty, insert, member, lookup, elems
) where 

import Prelude hiding (lookup, length)

import Control.Applicative((<$>))

import qualified System.Mem.StableName.Dynamic.Map as DM
import qualified System.Mem.StableName.Dynamic     as DM


data IndexMap a = IndexMap 
    { elems     :: [(a, Int)]
    , length    :: Int
    , dynMap    :: DM.Map Int }

empty :: IndexMap a
empty = IndexMap [] 0 DM.empty

insert :: a -> IndexMap a -> IO (IndexMap a)
insert v m = do
    isMember <- member v m
    if isMember 
        then return m
        else do name <- DM.makeDynamicStableName v            
                return $ IndexMap ((v, len) : elems m) (succ len) (DM.insert name len (dynMap m))
    where len = length m

member :: a -> IndexMap a -> IO Bool
member v m = flip DM.member (dynMap m) <$> DM.makeDynamicStableName v

lookup :: a -> IndexMap a -> IO (Maybe Int)
lookup a m = flip DM.lookup (dynMap m) <$> DM.makeDynamicStableName a

