module Csound.Exp.Instr(
    InstrFun, mkInstr, mkArity, saveInstr
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Csound.Exp
import Csound.Exp.SE
import Csound.Exp.Tuple
import Csound.Exp.Arg

import qualified Csound.Render.IndexMap as DM

saveInstr :: (Arg a, Out b) => (a -> b) -> SE DM.InstrName
saveInstr instrFun = SE $ do
    let (name, body) = mkInstr getArity toArg instrFun
    s <- get
    im <- lift $ DM.insert name body (instrMap s)
    put $ s { instrMap = im }
    return name
    where getArity = mkArity arity outArity

type InstrFun a b = a -> b

mkInstr :: Out b => (InstrFun a b -> Arity) -> a -> InstrFun a b -> (DM.InstrName, Instr)
mkInstr getArity arg instrFun = (DM.makeInstrName instrFun, x)
    where x = Instr (getArity instrFun) (toOut $ instrFun arg)
          

mkArity :: (a -> Int) -> (b -> Int) -> InstrFun a b -> Arity
mkArity ins outs instr = let (a, b) = funProxy instr in Arity (ins a) (outs b)
    where funProxy :: (a -> b) -> (a, b)
          funProxy = const (undefined, undefined)      


newTuple :: CsdTuple a => SE a
newTuple = undefined

