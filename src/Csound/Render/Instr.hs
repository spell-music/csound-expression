{-# Language TupleSections #-}
module Csound.Render.Instr(
    renderInstr
) where

import qualified Data.IntMap as IM
-- import Control.Monad.State
import Data.Char(toLower)
import Data.List(partition, sortBy)
import Control.Arrow(second)
import Data.Ord(comparing)
import Data.Maybe(fromJust)

import Text.PrettyPrint 

import Csound.Tfm.DAG 
import Csound.Tfm.BiMap
import Csound.Exp
import Csound.Exp.Wrapper hiding (double, int, var)

import Csound.Tfm.RateGraph
import Csound.Tfm.TfmTree
import Csound.Exp.BoolExp(renderCondInfo)
import Csound.Exp.NumExp(renderNumExp)
import Csound.Exp.Inline
import Csound.Render.Pretty
import Csound.Render.PrettyOp

type InstrId = Int

renderInstr :: KrateSet -> TabMap -> InstrId -> E -> Doc
renderInstr krateSet ft instrId exp = ppInstr instrId $ renderInstrBody krateSet ft exp

renderInstrBody :: KrateSet -> TabMap -> E -> Doc
renderInstrBody krateSet ft sig = vcat $ map (stmt . clearEmptyResults) $ collectRates krateSet st g
    where stmt :: ([RatedVar], Exp RatedVar) -> Doc
          stmt (res, exp) = ppOuts res <+> renderExp exp
          
          st = getRenderState g
          g  = toDag ft sig


data RenderState = RenderState 
    { multiOutsLinks :: IM.IntMap [MultiOutPort]
    , multiOutsRates :: [(Int, Rate)]    
    }

data MultiOutPort = MultiOutPort
    { idMultiOutPort    :: Int
    , orderMultiOutPort :: Int
    } 

getRenderState :: Dag RatedExp -> RenderState
getRenderState a = RenderState moLinks moRates
    where moLinks = IM.fromListWith (++) $ map extract selectInfo
          moRates = fmap (second getRate) selectInfo           

          selectInfo = filter (isSelect . ratedExpExp . snd) a    
            
          extract (n, x) = case ratedExpExp x of
                Select rate order parent -> (parent, [MultiOutPort n order])

filterMultiOutHelpers :: [(RatedVar, Exp RatedVar)] -> [(RatedVar, Exp RatedVar)]
filterMultiOutHelpers = filter (not . isSelect . snd) 

isSelect x = case x of
    Select _ _ _ -> True
    _ -> False


toDag :: TabMap -> E -> Dag RatedExp 
toDag ft exp = dag $ substTabs ft exp


clearEmptyResults :: ([RatedVar], Exp RatedVar) -> ([RatedVar], Exp RatedVar)
clearEmptyResults (res, exp) = (filter ((/= Xr) . ratedVarRate) res, exp)
        
collectRates :: KrateSet -> RenderState -> Dag RatedExp -> [([RatedVar], Exp RatedVar)]
collectRates krateSet st dag = evalState res lastFreshId  
    where res = tfmMultiRates st $ filterMultiOutHelpers dag1
          (dag1, lastFreshId) = grate krateSet dag


tfmMultiRates :: RenderState -> [(RatedVar, Exp RatedVar)] -> State Int [([RatedVar], Exp RatedVar)]
tfmMultiRates st as = mapM substRate as
    where substRate (n, exp) 
            | isMultiOutExp exp = fmap (,exp) $ getMultiOutVars (multiOutsLinks st IM.! ratedVarId n) exp
            | otherwise = return ([n], exp)

          isMultiOutExp x = case x of
              Tfm i _ -> isMultiOutSignature (infoSignature i)
              _ -> False
  
getMultiOutVars :: [MultiOutPort] -> Exp RatedVar -> State Int [RatedVar]
getMultiOutVars ports exp = fmap (zipWith RatedVar (getRates exp)) (getPorts ports)
    where getPorts ps = state $ \lastFreshId -> 
            let ps' = sortBy (comparing orderMultiOutPort) ps
                (ids, lastPortOrder) = runState (mapM (fillMissingPorts lastFreshId) ps') 0
                ids' = ids ++ [map (+ lastFreshId) [lastPortOrder + 1 .. portsSize - 1]]                
            in  (concat ids', lastFreshId + portsSize - inUsePortsSize)             
                  
                  
          rates = getRates exp
          portsSize = length rates    
          inUsePortsSize = length ports  
                    
            
          fillMissingPorts :: Int -> MultiOutPort -> State Int [Int]
          fillMissingPorts lastFreshId port = state $ \s ->
                if s == order
                then ([e], next) 
                else (fmap (+ lastFreshId) [s .. order - 1] ++ [e], next)
            where e = idMultiOutPort port
                  order = orderMultiOutPort port                  
                  next = order + 1
             

getRate :: RatedExp a -> Rate
getRate = fromJust . ratedExpRate

renderExp :: Exp RatedVar -> Doc
renderExp x = case fmap ppRatedVar x of
    ExpPrim (PString n) -> ppStrget n
    ExpPrim p -> assign $ ppPrim p
    Tfm info [a, b] | isInfix  info -> assign $ binary (infoName info) a b
    Tfm info xs     -> ppOpc (infoName info) xs
    ConvertRate to from x -> ppConvertRate to from x
    If info t e -> assign $ ppIf (renderCondInfo id info) t e
    ExpNum a -> assign $ renderNumExp id a
    WriteVar v a -> ppVar v <+> equals <+> a
    ReadVar v -> assign $ ppVar v
    x -> error $ "unknown expression: " ++ show x

 
    
