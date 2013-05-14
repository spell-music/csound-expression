{-# Language ScopedTypeVariables #-}
module Csound.Exp.Instr(
    soundSourceExp,
    effectExp,
    trigExp
) where

import Control.Monad(zipWithM, zipWithM_)
import Data.Foldable(foldMap)
import Data.List(transpose)

import qualified Data.Map as M


import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict

import Csound.Exp
import Csound.Exp.Wrapper
import Csound.Exp.SE
import Csound.Exp.GE
import Csound.Exp.Tuple
import Csound.Exp.Arg

import Csound.Render.Channel
import Csound.Tfm.Tab
import qualified Csound.Render.IndexMap as DM


funProxy :: (a -> b) -> (a, b)
funProxy = const (undefined, undefined)    

soundSourceExp :: (Arg a, Out b) => (a -> b) -> GE E
soundSourceExp instr = substTabs exp
    where insArity = arity $ fst $ funProxy instr
          exp = instrExp insArity $ toOut $ instr toArg

effectExp :: (Out a, Out b) => (a -> b) -> GE E
effectExp eff = substTabs $ mixerExp $ do
    inputs <- ins $ outArity $ fst $ funProxy eff
    toOut $ eff $ fromOut $ inputs
    

substTabs :: E -> GE E
substTabs exp = do
    opt <- options
    let exp' = defineInstrTabs (tabFi opt) exp
        tabs = getInstrTabs exp'
    ids <- mapM saveTab tabs
    let tabMap = M.fromList $ zip tabs ids
    return $ substInstrTabs tabMap exp'
   
trigExp :: (Arg a, Out b) => (NoSE b -> SE ()) -> (a -> b) -> GE E
trigExp writer instr = substTabs $ execSE $ 
    writer . toCsdTuple . fmap toE =<< (toOut $ instr toArg)

 

----------------------------------------------------------
-- simple instrument trigered with score

-- How to render an instrument
masterExp, mixerExp :: SE [Sig] -> E

-- 4 + arity because there are 3 first arguments (instrId, start, dur) and arity params comes next
masterExp  = instrExpGen masterOuts
mixerExp   = instrExpGen (outs 4) -- for mixing instruments we expect the port number to be the fourth parameter

instrExp :: Int -> SE [Sig] -> E
instrExp insArity = instrExpGen (outs (4 + insArity))

instrExpGen :: ([Sig] -> SE ()) -> SE [Sig] -> E
instrExpGen formOuts instrBody = execSE $ formOuts =<< instrBody



-- other outputs

outs :: Int -> [Sig] -> SE ()
outs readChnId sigs = zipWithM_ (out readChnId) [1 .. ] sigs
    where out readChnId n sig = chnmix sig $ chnName n (p readChnId) 

-- inputs

ins :: Int -> SE [Sig]
ins n = mapM in_ [1 .. n] 
    where in_ n = do
              let name = chnName n $ readVar chnVar
              sig <- chnget name
              chnclear name
              return sig


