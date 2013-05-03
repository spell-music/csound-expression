{-# Language TupleSections #-}
module Csound.Render.Instr(
    renderInstr, renderInstrBody
) where

import Control.Arrow(first, second)
import qualified Data.IntMap as IM
import Control.Monad.Trans.State.Strict
import Data.Char(toLower)
import Data.List(partition, sortBy, sort, find)
import Control.Arrow(second)
import Data.Ord(comparing)
import Data.Maybe(fromJust)
import qualified Data.Map as M

import Data.Default
import Data.Fix(Fix(..), cata)
import Data.Fix.Cse(fromDag, cse)

import Csound.Exp
import Csound.Exp.Wrapper(getRates, isMultiOutSignature, toE, noRate)

import Csound.Tfm.DeduceTypes
import Csound.Render.Pretty(ppStmt, ppInstr, Doc)

type InstrId = Int
type Dag f = [(Int, f Int)]

renderInstr :: InstrId -> E -> Doc
renderInstr instrId exp = ppInstr instrId $ renderInstrBody exp

renderInstrBody :: E -> [Doc]
renderInstrBody sig = map (uncurry ppStmt . clearEmptyResults) $ collectRates $ toDag sig

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
        
collectRates :: Dag RatedExp -> [([RatedVar], Exp RatedVar)]
collectRates dag = evalState res lastFreshId  
    where st = getRenderState dag    
          res = tfmMultiRates st $ filterMultiOutHelpers dag1
          (dag1, lastFreshId) = rateGraph dag

          rateGraph dag = (fmap (second ratedExpExp) stmts, lastId)
             where (stmts, lastId) = deduceTypes algSpec dag
                   algSpec = TypeGraph mkConvert' defineType'

                   mkConvert' a = (to, RatedExp def def $ ConvertRate (ratedVarRate to) (ratedVarRate from) $ PrimOr $ Right from)
                       where from = convertFrom a
                             to   = convertTo   a

                   defineType' (outVar, expr) desiredRates = (ratesForConversion, (outVar', expr'))
                       where possibleRate = deduceRate desiredRates expr 
                             ratesForConversion = filter (not . flip coherentRates possibleRate) desiredRates
                             expr' = RatedExp def def $ rateExp possibleRate $ ratedExpExp expr
                             outVar' = ratedVar possibleRate outVar


coherentRates :: Rate -> Rate -> Bool
coherentRates to from = case (to, from) of
    (a, b)  | a == b    -> True
    (Xr, a)             -> True             
    (Kr, Ir)            -> True
    _                   -> False

deduceRate :: [Rate] -> RatedExp Int -> Rate
deduceRate desiredRates exp = case ratedExpExp exp of
    ExpPrim p -> case desiredRates of
        [Sr] -> Sr
        _ -> Ir
       
    Tfm info as | isProcedure info -> Xr
    Tfm info as -> case infoSignature info of
        MultiRate _ _ -> Xr
        SingleRate tab -> 
            let r1 = tfmNoRate (infoName info) desiredRates tab
            in  case ratedExpRate exp of
                    Just r | M.member r tab -> r
                    Just r -> r1
                    Nothing -> r1
    
    ExpNum _ -> case maximum desiredRates of
        Xr -> Ar
        r -> r
    
    Select rate _ _ -> rate
    If info a b -> head $ filter (/= Xr) $ sort desiredRates   
    ReadVar v -> varRate v
    WriteVar _ _ -> Xr    
    where tfmNoRate name desiredRates tab = case sort desiredRates of
              [Xr]  -> tfmNoRate name [Ar] tab                
              Xr:as -> tfmNoRate name as tab
              as -> fromJust $ find (flip M.member tab) (as ++ [minBound .. maxBound])         

rateExp :: Rate -> Exp Int -> Exp RatedVar 
rateExp curRate exp = case exp of
    ExpPrim (P n) | curRate == Sr -> ExpPrim (PString n)
    Tfm i xs -> Tfm i $ mergeWithPrimOrBy (flip ratedVar) xs (ratesFromSignature curRate (infoSignature i))
    Select rate pid a -> Select rate pid (fmap (ratedVar rate) a)    
    If condInfo a' b' -> let curRate' = max curRate Kr
                         in  fmap (fmap (ratedVar curRate')) exp
    ExpNum _ -> fmap (fmap (ratedVar curRate)) exp    
    ReadVar v -> ReadVar v
    WriteVar v a -> WriteVar v $ fmap (ratedVar (varRate v)) a
    ExpPrim p -> ExpPrim p
    where ratesFromSignature rate signature = case signature of
              SingleRate table -> table M.! rate
              MultiRate _ rs   -> rs

mergeWithPrimOrBy :: (a -> b -> c) -> [PrimOr a] -> [b] -> [PrimOr c]
mergeWithPrimOrBy cons = zipWith (\primOr b -> fmap (flip cons b) primOr)

tfmMultiRates :: RenderState -> [(RatedVar, Exp RatedVar)] -> State Int [([RatedVar], Exp RatedVar)]
tfmMultiRates st as = mapM substRate as
    where substRate (n, exp) 
            | isMultiOutExp exp = fmap (,exp) $ getMultiOutVars (multiOutsLinks st IM.! ratedVarId n) (getRates exp)
            | otherwise = return ([n], exp)

          isMultiOutExp x = case x of
              Tfm i _ -> isMultiOutSignature (infoSignature i)
              _ -> False
  
getMultiOutVars :: [MultiOutPort] -> [Rate] -> State Int [RatedVar]
getMultiOutVars ports rates = fmap (zipWith ratedVar rates) (getPorts ports)
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

 
    
