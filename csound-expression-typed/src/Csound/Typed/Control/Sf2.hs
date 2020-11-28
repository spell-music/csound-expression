{-# Language TypeFamilies #-}
module Csound.Typed.Control.Sf2(
    Sf(..), unSf
) where

import Data.Boolean
import Data.Default

import qualified Csound.Dynamic as D

import Csound.Typed.Types
import Csound.Typed.GlobalState

-- | The sf2 sound font preset. It is defined with
-- file name, bank and program integers.
data Sf = Sf 
    { sfName :: String
    , sfBank :: Int
    , sfProg :: Int } 
    | SfId (GE E)
    
instance Val Sf where
    fromGE = SfId
    toGE   = unSf

unSf :: Sf -> GE E
unSf x = case x of
    SfId a -> a
    Sf name bank prog -> fmap D.int $ saveSf (SfSpec name bank prog)

instance Default Sf where 
    def = fromE 0

type instance BooleanOf Sf  = BoolD
instance IfB Sf where ifB = on3 ifB

