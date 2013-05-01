-- render strings
module Csound.Tfm.String(
    stringMap, substNoteStrs        
) where

import Data.List(nub)
import qualified Data.Map as M(fromList, (!))

import Csound.Exp(StringMap, Prim(..), Note)

stringMap :: [Prim] -> StringMap
stringMap as = M.fromList $ zip (nub $ primStrings =<< as) [1 .. ]
    where primStrings x = case x of
              PrimString s -> [s]
              _ -> []

substNoteStrs :: StringMap -> Note -> Note
substNoteStrs m = fmap (substPrimStrs m)

substPrimStrs :: StringMap -> Prim -> Prim
substPrimStrs strs x = case x of
    PrimString s -> PrimInt $ strs M.! s
    _ -> x

