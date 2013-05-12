{-# Language ScopedTypeVariables #-}
module Csound.Exp.Instr(
--    InstrFun, mkInstr, mkArity, saveInstr, saveTrigInstr,
--    newCsdTuple
) where

import Control.Monad(zipWithM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Csound.Exp
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Tuple
import Csound.Exp.Arg

import Csound.Render.Channel(instrExp)
import qualified Csound.Render.IndexMap as DM

{-
saveInstr :: (Arg a, Out b) => (a -> b) -> GE InstrId
saveInstr instrFun = GE $ do
    s <- get
    let name = DM.makeInstrName instrFun
    maybeInstrId <- lift $ DM.lookup name (instrSet s)
    case maybeInstrId of
        Just n  -> return $ intInstrId n
        Nothing -> do
            (n, instrSet') <- lift $ DM.insert name (instrSet s)
            let instrBody = toOut $ instrFun toArg  
                exp = instrExp (insArity instrFun) instrBody
            put $ s{ instrSet = instrSet', instrMap = (intInstrId n, exp) : instrMap s }
            return $ intInstrId n
    where insArity = arity . fst . funProxy

saveTrigInstr :: InstrId -> (Int -> E) -> GE ()
saveTrigInstr = undefined {-name exp = SE $ modify $ 
    \s -> s{ trigMap = TrigInstrMap (TrigInstr name exp : unTrigInstrMap (trigMap s)) }
    -}

type InstrFun a b = a -> b

mkInstr :: Out b => (InstrFun a b -> Arity) -> a -> InstrFun a b -> (DM.InstrName, Instr)
mkInstr getArity arg instrFun = (DM.makeInstrName instrFun, x)
    where x = Instr (getArity instrFun) (toOut $ instrFun arg)
          

mkArity :: (a -> Int) -> (b -> Int) -> InstrFun a b -> Arity
mkArity ins outs instr = let (a, b) = funProxy instr in Arity (ins a) (outs b)

funProxy :: (a -> b) -> (a, b)
funProxy = const (undefined, undefined)      

newCsdTuple :: forall a . CsdTuple a => GE a
newCsdTuple = fmap toCsdTuple $ 
    zipWithM (\a b -> fmap readVar $ newGlobalVar a b) (ratesCsdTuple x) (fromCsdTuple x)
    where x = defCsdTuple :: a
-}
