module Csound.Tfm.TfmTree where

import Data.List(nub)
import Data.Fix
import qualified Data.Map as M
import Data.Foldable(foldMap)

import Csound.Exp

type FtableMap = M.Map Ftable Int

-- substitute ftables for ids 

substFtables :: FtableMap -> E -> E
substFtables m = cata $ \(RatedExp r x) -> Fix $ RatedExp r $ case x of
    ExpPrim (PrimFtable t) -> ExpPrim (PrimInt $ m M.! t)
    _ -> x


ftableMap :: [E] -> FtableMap
ftableMap es = M.fromList $ zip (nub $ getFtables =<< es) [1 ..]

getFtables :: E -> [Ftable]
getFtables = cata $ \(RatedExp _ x) -> case x of
    ExpPrim (PrimFtable t) -> [t]
    ExpPrim _ -> []
    Tfm _ as -> concat as
    ConvertRate _ _ a -> a
    Select _ a -> a
    If info a b -> foldMap id info ++ a ++ b
    Outs as -> concat as
    ExpBuf op deps a -> concat $ a:deps 
    Depends deps a -> concat $ a:deps
    Var _ _ _ -> []
