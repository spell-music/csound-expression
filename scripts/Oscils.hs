module Main where

import Csound.Core

main = writeCsd "tmp.csd" $ fromMono (tri 220)
