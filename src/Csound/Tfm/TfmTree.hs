module Csound.Tfm.TfmTree(
    tabMap
) where

import Data.List(nub)
import Data.Fix
import qualified Data.Map as M
import Data.Foldable(foldMap)

import Csound.Exp

tabMap :: [E] -> TabMap
tabMap es = M.fromList $ zip (nub $ getFtables =<< es) [1 ..]

getFtables :: E -> [Tab]
getFtables = cata $ \re -> case fmap fromPrimOr $ ratedExpExp re of    
    ExpPrim p -> fromPrim p
    Tfm _ as -> concat as
    ConvertRate _ _ a -> a
    ExpNum a -> foldMap id a
    Select _ _ a -> a
    If info a b -> foldMap id info ++ a ++ b
    ReadVar _ -> []
    WriteVar _ a -> a
    where fromPrim x = case x of
            PrimTab t -> [t]
            _ -> []
          fromPrimOr x = case unPrimOr x of
            Left  p -> fromPrim p
            Right a -> a

