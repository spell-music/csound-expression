{-# Language TupleSections #-}
module Csound.Render.Instr(
    renderInstr, renderInstrBody
) where

import qualified Data.IntMap as IM
import Control.Monad.Trans.State.Strict
import Data.Char(toLower)
import Data.List(partition, sortBy)
import Control.Arrow(second)
import Data.Ord(comparing)
import Data.Maybe(fromJust)

import Data.Fix(Fix(..), cata)
import Data.Fix.Cse(fromDag, cse)

import Csound.Exp
import Csound.Exp.Wrapper(getRates, isMultiOutSignature)

import Csound.Tfm.RateGraph
import Csound.Render.Pretty

type InstrId = Int
type Dag f = [(Int, f Int)]

renderInstr :: InstrId -> E -> Doc
renderInstr instrId exp = ppInstr instrId $ renderInstrBody exp

renderInstrBody :: E -> [Doc]
renderInstrBody sig = map (stmt . clearEmptyResults) $ collectRates st g
    where stmt :: ([RatedVar], Exp RatedVar) -> Doc
          stmt (res, exp) = renderExp (ppOuts res) exp
          
          st = getRenderState g
          g  = toDag sig


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
                Select rate order (PrimOr (Right parent)) -> (parent, [MultiOutPort n order])

filterMultiOutHelpers :: [(RatedVar, Exp RatedVar)] -> [(RatedVar, Exp RatedVar)]
filterMultiOutHelpers = filter (not . isSelect . snd) 

isSelect x = case x of
    Select _ _ _ -> True
    _ -> False


toDag :: E -> Dag RatedExp 
toDag exp = fromDag $ cse $ trimByArgLength exp


trimByArgLength :: E -> E
trimByArgLength = cata $ \x -> Fix x{ ratedExpExp = phi $ ratedExpExp x }
    where phi x = case x of
            Tfm info xs -> Tfm (info{infoSignature = trimInfo (infoSignature info) xs}) xs
            _ -> x
          trimInfo signature args = case signature of
            SingleRate tab -> SingleRate $ fmap trim tab
            MultiRate outs ins -> MultiRate outs (trim ins)        
            where trim = take (length args)    
                  

clearEmptyResults :: ([RatedVar], Exp RatedVar) -> ([RatedVar], Exp RatedVar)
clearEmptyResults (res, exp) = (filter ((/= Xr) . ratedVarRate) res, exp)
        
collectRates :: RenderState -> Dag RatedExp -> [([RatedVar], Exp RatedVar)]
collectRates st dag = evalState res lastFreshId  
    where res = tfmMultiRates st $ filterMultiOutHelpers dag1
          (dag1, lastFreshId) = grate dag


tfmMultiRates :: RenderState -> [(RatedVar, Exp RatedVar)] -> State Int [([RatedVar], Exp RatedVar)]
tfmMultiRates st as = mapM substRate as
    where substRate (n, exp) 
            | isMultiOutExp exp = fmap (,exp) $ getMultiOutVars (multiOutsLinks st IM.! ratedVarId n) (getRates exp)
            | otherwise = return ([n], exp)

          isMultiOutExp x = case x of
              Tfm i _ -> isMultiOutSignature (infoSignature i)
              _ -> False
  
getMultiOutVars :: [MultiOutPort] -> [Rate] -> State Int [RatedVar]
getMultiOutVars ports rates = fmap (zipWith RatedVar rates) (getPorts ports)
    where getPorts ps = state $ \lastFreshId -> 
            let ps' = sortBy (comparing orderMultiOutPort) ps
                (ids, lastPortOrder) = runState (mapM (fillMissingPorts lastFreshId) ps') 0
                ids' = ids ++ [map (+ lastFreshId) [0 .. portsSize - 1 - lastPortOrder]]                
            in  (concat ids', lastFreshId + portsSize - inUsePortsSize)                             

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

renderExp :: Doc -> Exp RatedVar -> Doc
renderExp res exp = case fmap ppPrimOrVar exp of
    ExpPrim (PString n) -> ppStrget res n
    ExpPrim p -> res $= ppPrim p
    Tfm info [a, b] | isInfix  info -> res $= binary (infoName info) a b
    Tfm info xs -> ppOpc res (infoName info) xs
    ConvertRate to from x -> ppConvertRate res to from x
    If info t e -> res $= ppIf (ppInline ppCondOp info) t e
    ExpNum (PreInline op as) -> res $= ppNumOp op as
    WriteVar v a -> ppVar v $= a
    ReadVar v -> res $= ppVar v
    x -> error $ "unknown expression: " ++ show x

 
    
