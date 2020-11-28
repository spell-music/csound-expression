-- | Defines functions to read global arguments from the command line as macros with D flag.
module Csound.Typed.Control.MacrosArgs(
    readMacrosString, readMacrosDouble, readMacrosInt
) where

import qualified Csound.Dynamic as D

import Csound.Typed.Types
import qualified Csound.Typed.GlobalState as G(readMacrosString, readMacrosDouble, readMacrosInt)

readMacrosString :: String -> String -> Str
readMacrosString name value = fromGE $ G.readMacrosString name value 

readMacrosDouble :: String -> Double -> D
readMacrosDouble name value = fromGE $ G.readMacrosDouble name value 

readMacrosInt :: String -> Int -> D
readMacrosInt name value = fromGE $ G.readMacrosInt  name value
