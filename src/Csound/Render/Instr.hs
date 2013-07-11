module Csound.Render.Instr(
    renderInstr, renderInstrBody
) where

import Control.Arrow(second)
import Data.List(sort, find)
import qualified Data.Map as M

import Data.Maybe(fromJust)
import Data.Default
import Data.Fix(Fix(..), cata)
import Data.Fix.Cse(fromDag, cse)

import Csound.Exp hiding (Var)
import Csound.Exp.Wrapper(getRates, isMultiOutSignature)

import Csound.Tfm.DeduceTypes
import Csound.Tfm.UnfoldMultiOuts
import Csound.Render.Pretty(ppStmt, ppInstr, vcat, Doc)

type Dag f = [(Int, f Int)]

renderInstr :: InstrId -> E -> Doc
renderInstr instrId expr = ppInstr instrId $ renderInstrBody expr

renderInstrBody :: E -> Doc
renderInstrBody sig = vcat $ map (uncurry ppStmt . clearEmptyResults) $ collectRates $ toDag sig

-------------------------------------------------------------
-- E -> Dag

toDag :: E -> Dag RatedExp 
toDag expr = fromDag $ cse $ trimByArgLength expr

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
clearEmptyResults (res, expr) = (filter ((/= Xr) . ratedVarRate) res, expr)
        
collectRates :: Dag RatedExp -> [([RatedVar], Exp RatedVar)]
collectRates dag = fmap (second ratedExpExp) res
    where res = unfoldMultiOuts unfoldSpec lastFreshId dag1  
          (dag1, lastFreshId) = rateGraph dag

-----------------------------------------------------------
-- Dag -> Dag

-----------------------------------------------------------
-- deduces types

rateGraph :: [Stmt RatedExp Int] -> ([Stmt RatedExp (Var Rate)], Int)
rateGraph dag = (stmts, lastId)
     where (stmts, lastId) = deduceTypes algSpec dag
           algSpec = TypeGraph mkConvert' defineType'

           mkConvert' a = (to, RatedExp def def $ 
                   ConvertRate (ratedVarRate to) (ratedVarRate from) $ PrimOr $ Right from)
               where from = convertFrom a
                     to   = convertTo   a

           defineType' (outVar, expr) desiredRates = (ratesForConversion, (outVar', expr'))
               where possibleRate = deduceRate desiredRates expr 
                     ratesForConversion = filter (not . flip coherentRates possibleRate) desiredRates
                     expr' = RatedExp def def $ rateExp possibleRate $ ratedExpExp expr
                     outVar' = ratedVar possibleRate outVar

----------------------------------------------------------
-- unfolds multiple rates

unfoldSpec :: UnfoldMultiOuts RatedExp Rate 
unfoldSpec = UnfoldMultiOuts getSelector' getParentTypes'
    where getSelector' x = case ratedExpExp x of
                Select _ order (PrimOr (Right parent)) -> Just $ Selector parent order 
                _ -> Nothing
          getParentTypes' x = case ratedExpExp x of
                Tfm i _ -> if (isMultiOutSignature $ infoSignature i) 
                           then Just (getRates $ ratedExpExp x) 
                           else Nothing 
                _ -> Nothing

coherentRates :: Rate -> Rate -> Bool
coherentRates to from = case (to, from) of
    (a, b)  | a == b    -> True
    (Xr, _)             -> True             
    (Kr, Ir)            -> True
    _                   -> False

deduceRate :: [Rate] -> RatedExp Int -> Rate
deduceRate desiredRates expr = case ratedExpExp expr of
    ExpPrim _ -> case desiredRates of
        [Sr] -> Sr
        _ -> Ir
       
    Tfm info _ | isProcedure info -> Xr
    Tfm info _ -> case infoSignature info of
        MultiRate _ _ -> Xr
        SingleRate tab -> 
            let r1 = tfmNoRate (infoName info) desiredRates tab
            in  case ratedExpRate expr of
                    Just r | M.member r tab -> r
                    Just _ -> r1
                    Nothing -> r1
    
    ExpNum _ -> case maximum desiredRates of
        Xr -> Ar
        r -> r
    
    Select rate _ _ -> rate
    If _ _ _ -> head $ filter (/= Xr) $ sort desiredRates   
    ReadVar v -> varRate v
    WriteVar _ _ -> Xr    
    InitVar _ _ -> Xr    
    where tfmNoRate name rates tab = case sort rates of
              [Xr]  -> tfmNoRate name [Ar] tab                
              Xr:as -> tfmNoRate name as tab
              as -> fromJust $ find (flip M.member tab) (as ++ [minBound .. maxBound])         

rateExp :: Rate -> Exp Int -> Exp RatedVar 
rateExp curRate expr = case expr of
    ExpPrim (P n) | curRate == Sr -> ExpPrim (PString n)
    Tfm i xs -> Tfm i $ mergeWithPrimOrBy (flip ratedVar) xs (ratesFromSignature curRate (infoSignature i))
    Select rate pid a -> Select rate pid (fmap (ratedVar Xr) a)    
    If _ _ _ -> let curRate' = max curRate Kr
                in  fmap (fmap (ratedVar curRate')) expr
    ExpNum _ -> fmap (fmap (ratedVar curRate)) expr    
    ReadVar v -> ReadVar v
    WriteVar v a -> WriteVar v $ fmap (ratedVar (varRate v)) a
    InitVar v a -> InitVar v $ fmap (ratedVar (varRate v)) a
    ExpPrim p -> ExpPrim p
    where ratesFromSignature rate signature = case signature of
              SingleRate table -> table M.! rate
              MultiRate _ rs   -> rs

mergeWithPrimOrBy :: (a -> b -> c) -> [PrimOr a] -> [b] -> [PrimOr c]
mergeWithPrimOrBy cons = zipWith (\primOr b -> fmap (flip cons b) primOr)

