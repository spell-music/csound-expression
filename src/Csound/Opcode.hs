module Csound.Opcode (
    idur, zeroDbfs, xtratim,
    module Csound.Opcode.Basic,
    module Csound.Opcode.Advanced,
    module Csound.Opcode.Data,
    module Csound.Opcode.Interaction
) where

import qualified Data.Map as M

import Csound.Exp.Cons
import Csound.Render.Sco(Msg)
import Csound.Exp.Wrapper hiding (Double', Int', String')
import Csound.Exp

import Csound.Opcode.Basic
import Csound.Opcode.Advanced
import Csound.Opcode.Data
import Csound.Opcode.Interaction

-- handy shortcuts

i = Ir
k = Kr
a = Ar
x = Xr
s = Sr
          
idur :: D
idur = p 3
        
zeroDbfs :: D
zeroDbfs = (setRate Ir :: E -> D) $ readVar (VarVerbatim Ir "0dbfs")

xtratim :: D -> SE ()
xtratim a1 = se_ $ opc1 "xtratim" [(x, [i])] a1




