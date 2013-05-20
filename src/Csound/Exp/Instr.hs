{-# Language ScopedTypeVariables #-}
module Csound.Exp.Instr(
    soundSourceExp,
    effectExp,
    trigExp
) where

import Control.Monad(zipWithM_)
import qualified Data.Map as M

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Tuple
import Csound.Exp.Arg
import Csound.Exp.Options

import Csound.Render.Channel
import Csound.Tfm.Tab


funProxy :: (a -> b) -> (a, b)
funProxy = const (undefined, undefined)    

soundSourceExp :: (Arg a, Out b) => (a -> b) -> GE E
soundSourceExp instr = substTabs expr
    where insArity = arity $ fst $ funProxy instr
          expr = instrExp insArity $ toOut $ instr toArg

effectExp :: (Out a, Out b) => (a -> b) -> GE E
effectExp eff = substTabs $ mixerExp $ do
    inputs <- ins $ outArity $ fst $ funProxy eff
    toOut $ eff $ fromOut $ inputs
    

substTabs :: E -> GE E
substTabs expr = do
    opt <- getOptions
    let expr' = defineInstrTabs (tabFi opt) expr
        tabs  = getInstrTabs expr'
    ids <- mapM saveTab tabs
    let tabMap = M.fromList $ zip tabs ids
    return $ substInstrTabs tabMap expr'
   
trigExp :: (Arg a, Out b) => (NoSE b -> SE ()) -> (a -> b) -> GE E
trigExp writer instr = substTabs $ execSE $ 
    writer . toCsdTuple . fmap toE =<< (toOut $ instr toArg)


----------------------------------------------------------
-- simple instrument trigered with score

-- How to render an instrument
mixerExp :: SE [Sig] -> E

-- 4 + arity because there are 3 first arguments (instrId, start, dur) and arity params comes next
mixerExp   = instrExpGen (outs 4) -- for mixing instruments we expect the port number to be the fourth parameter

instrExp :: Int -> SE [Sig] -> E
instrExp insArity = instrExpGen (outs (4 + insArity))

instrExpGen :: ([Sig] -> SE ()) -> SE [Sig] -> E
instrExpGen formOuts instrBody = execSE $ formOuts =<< instrBody



-- other outputs

outs :: Int -> [Sig] -> SE ()
outs readChnId sigs = zipWithM_ (out readChnId) [1 .. ] sigs
    where out chnId n asig = chnmix asig $ chnName n (p chnId) 

-- inputs

ins :: Int -> SE [Sig]
ins n = mapM in_ [1 .. n] 
    where in_ x = do
              let name = chnName x $ readVar chnVar
              asig <- chnget name
              chnclear name
              return asig


