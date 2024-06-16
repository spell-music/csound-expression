module Csound.Dynamic.Debug
  ( IsDebug (..)
  , traceIf
  , traceShowIf
  ) where

import Debug.Trace (trace, traceShow)
import Data.Default

newtype IsDebug = IsDebug Bool
  deriving newtype (Show, Eq, Ord, Read)

instance Default IsDebug where
  def = IsDebug False

traceIf :: IsDebug -> String -> a -> a
traceIf (IsDebug isDebug) str a
  | not isDebug = a
  | otherwise = trace str a

traceShowIf :: Show b => IsDebug -> b -> a -> a
traceShowIf (IsDebug isDebug) debugValue a
  | not isDebug = a
  | otherwise   = traceShow debugValue a
