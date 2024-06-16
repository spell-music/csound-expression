-- | Defines functions to read global arguments from the command line as macros with D flag.
module Csound.Typed.Control.MacrosArgs(
    readMacrosString, readMacrosDouble, readMacrosInt
) where

import Csound.Typed.Types
import qualified Csound.Typed.GlobalState as G(readMacrosString, readMacrosDouble, readMacrosInt)
import Data.Text (Text)

readMacrosString :: Text -> Text -> Str
readMacrosString name value = fromGE $ G.readMacrosString name value

readMacrosDouble :: Text -> Double -> D
readMacrosDouble name value = fromGE $ G.readMacrosDouble name value

readMacrosInt :: Text -> Int -> D
readMacrosInt name value = fromGE $ G.readMacrosInt  name value
