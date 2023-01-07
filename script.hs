module Main where

import Csound.Base
import Csound.Typed
import Csound.Typed.GlobalState.GE
import Csound.Dynamic.Render.Pretty
import Data.Functor

main = writeCsd "tmp.csd" res

res = do
  r <- newRef (0 :: Sig)
  let cps = 220 * linseg [1,2]
  whens
    [(sig getSampleRate >* 2, writeRef r (osc cps))
    , (sig getSampleRate >* 1, writeRef r (linseg [0, 1, 1] * (osc (2 * cps))))
    , (sig getSampleRate >* 0, writeRef r (linseg [0, 1, 1] * (osc 880)))
    ]
      (writeRef r $ tri cps)
  readRef r

main2 = do
  r <- to res
  print $ ppE r

to = evalGE def . execSE . void
