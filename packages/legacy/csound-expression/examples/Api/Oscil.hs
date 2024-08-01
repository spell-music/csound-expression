module Main where

import Csound.Base

volume = text "volume"
frequency = text "frequency"

instr = do
  vol <- chnGetCtrl volume
  cps <- chnGetCtrl frequency
  return (vol * osc cps)

main = writeCsd "osc.csd" instr
