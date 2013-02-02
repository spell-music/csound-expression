module Csound.Tfm.TfmTree where

import Data.List(nub)
import Data.Fix
import qualified Data.Map as M
import Data.Foldable(foldMap)

import Csound.Exp
import Csound.Exp.Inline

type TabMap = M.Map Tab Int

-- substitute ftables for ids 

substTabs :: TabMap -> E -> E
substTabs m = cata $ \(RatedExp r d x) -> Fix $ RatedExp r d $ case x of
    ExpPrim (PrimTab t) -> ExpPrim (PrimInt $ m M.! t)
    _ -> x


tabMap :: [E] -> TabMap
tabMap es = M.fromList $ zip (nub $ getFtables =<< es) [1 ..]

getFtables :: E -> [Tab]
getFtables = cata $ \(RatedExp _ _ x) -> case x of
    ExpPrim (PrimTab t) -> [t]
    ExpPrim _ -> []
    Tfm _ as -> concat as
    ConvertRate _ _ a -> a
    ExpNum a -> foldMap id a
    Select _ _ a -> a
    If info a b -> foldMap id info ++ a ++ b
    ReadVar _ -> []
    WriteVar _ a -> a
