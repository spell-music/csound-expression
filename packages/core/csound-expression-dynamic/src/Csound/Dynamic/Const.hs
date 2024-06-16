-- | Constants
module Csound.Dynamic.Const
  ( controlOpcodes
  , audioOpcodes
  ) where

import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Text

-- | Envelope generators are Kr by default
controlOpcodes :: HashSet Text
controlOpcodes =
  HashSet.fromList
    [ "adsr"
    , "madsr"
    , "xadsr"
    , "linen"
    , "linenr"
    , "envlpx"
    , "envlpxr"
    , "expon"
    , "expseg"
    , "expsegr"
    , "jspline"
    , "line"
    , "linseg"
    , "linsegr"
    , "rspline"
    , "transeg"
    , "bpf"
    , "bpfcos"
    ]

-- | Oscillators and table access are Ar by default
audioOpcodes :: HashSet Text
audioOpcodes =
  HashSet.fromList
    [ "oscil"
    , "oscili"
    , "oscil3"
    , "poscil"
    , "poscil3"
    , "oscilikt"
    , "table"
    , "tablei"
    , "table3"
    , "tab"
    , "tabw"
    ]

