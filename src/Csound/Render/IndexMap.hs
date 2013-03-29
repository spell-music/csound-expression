module Csound.Render.IndexMap(
    InstrName, makeInstrName,
    IndexMap, empty, insert, member, lookup, elems, length
) where 

import Prelude hiding (lookup, length)

import Control.Applicative((<$>))

import qualified System.Mem.StableName.Dynamic.Map as DM
import qualified System.Mem.StableName.Dynamic     as DM

type InstrName = IO DM.DynamicStableName

makeInstrName :: a -> InstrName
makeInstrName = DM.makeDynamicStableName

data IndexMap a = IndexMap 
    { elems     :: [(a, Int)]
    , length    :: Int
    , dynMap    :: DM.Map Int }

empty :: IndexMap a
empty = IndexMap [] 0 DM.empty

insert :: InstrName -> a -> IndexMap a -> IO (IndexMap a)
insert mname v m = do
    name <- mname
    isMember <- member mname m
    if isMember 
        then return m
        else return $ IndexMap ((v, len) : elems m) (succ len) (DM.insert name len (dynMap m))
    where len = length m

member :: InstrName -> IndexMap a -> IO Bool
member v m = flip DM.member (dynMap m) <$> v

lookup :: InstrName -> IndexMap a -> IO (Maybe Int)
lookup a m = flip DM.lookup (dynMap m) <$> a

