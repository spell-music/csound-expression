module Csound.Render.IndexMap(
    InstrName, makeInstrName,
    IndexMap, empty, insert, lookup
) where 

import Prelude hiding (lookup, length)

import qualified System.Mem.StableName.Dynamic.Map as DM(Map, empty, insert, member, lookup)
import qualified System.Mem.StableName.Dynamic     as DM(DynamicStableName, makeDynamicStableName)

type InstrName = IO DM.DynamicStableName

makeInstrName :: a -> InstrName
makeInstrName = DM.makeDynamicStableName

data IndexMap = IndexMap 
    { counter   :: Int    
    , dynMap    :: DM.Map Int }

empty :: Int -> IndexMap
empty startId = IndexMap startId DM.empty

insert :: InstrName -> IndexMap -> IO (Int, IndexMap)
insert mname m = do
    name <- mname
    mn <- lookup mname m
    return $ case mn of
        Just index -> (index, m)
        Nothing    -> (n, IndexMap (succ n) (DM.insert name n $ dynMap m))
    where n = counter m  

member :: InstrName -> IndexMap -> IO Bool
member v m = fmap (flip DM.member (dynMap m)) $ v

lookup :: InstrName -> IndexMap -> IO (Maybe Int)
lookup a m = fmap (flip DM.lookup (dynMap m)) $ a

