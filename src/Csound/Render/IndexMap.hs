module Csound.Render.IndexMap(
    InstrName, makeInstrName,
    IndexMap, empty, insert, member, lookup, elems, length
) where 

import Prelude hiding (lookup, length)

import qualified System.Mem.StableName.Dynamic.Map as DM(Map, empty, insert, member, lookup)
import qualified System.Mem.StableName.Dynamic     as DM(DynamicStableName, makeDynamicStableName)

type InstrName = IO DM.DynamicStableName

makeInstrName :: a -> InstrName
makeInstrName = DM.makeDynamicStableName

data IndexMap a = IndexMap 
    { elems     :: [(a, Int)]
    , counter   :: Int    
    , length    :: Int
    , dynMap    :: DM.Map Int }

empty :: Int -> IndexMap a
empty startId = IndexMap [] startId 0 DM.empty

insert :: InstrName -> a -> IndexMap a -> IO (IndexMap a)
insert mname v m = do
    name <- mname
    isMember <- member mname m
    if isMember 
        then return m
        else return $ IndexMap ((v, n) : elems m) (succ n) (succ len) (DM.insert name n $ dynMap m)
    where len = length m
          n   = counter m  

member :: InstrName -> IndexMap a -> IO Bool
member v m = fmap (flip DM.member (dynMap m)) $ v

lookup :: InstrName -> IndexMap a -> IO (Maybe Int)
lookup a m = fmap (flip DM.lookup (dynMap m)) $ a

