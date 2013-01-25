module Csound.Tfm.Rate where

import Data.Maybe(fromJust)
import Data.List(find)
import Data.Set(Set)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntMap as IM

import Data.Fix
import Csound.Tfm.DAG(Dag)

import Csound.Exp
import Csound.Exp.Wrapper(setRate, noRate, withRate, ratedExp)
import Csound.Exp.Cons(opc1)

import Debug.Trace

echo :: Show a => a -> a
echo a = trace (show a) a


type KrateSet = Set Name

defaultKrateSet :: KrateSet
defaultKrateSet = S.fromList ["linseg", "expseg"]

rate :: KrateSet -> Rate -> E -> E
rate krateSet rootRate a = case unFix a of
    RatedExp r d exp -> case exp of
        ExpPrim prim -> ratePrim rootRate prim
        Tfm info as  ->    
            let desiredRate = maybe (defaultRate rootRate krateSet $ infoName info) id r             
            in  convert rootRate desiredRate $
                rateTfm krateSet (possibleRate desiredRate $ infoSignature info) info as
        Select n a -> ratedExp r $ Select n $ rate krateSet (fromJust r) a
        If info a b -> withRate rootRate $ If info (rateRoot a) (rateRoot b)
    where rateRec   = rate krateSet  
          rateRoot  = rateRec rootRate         
   

ratePrim :: Rate -> Prim -> E
ratePrim rootRate p
    | isParam p && rootRate == Sr = rateStringParam $ rateParam p
    | isParam p = convert rootRate Ir $ rateParam p 
    | otherwise = withRate (handleX rootRate) (ExpPrim p)
    where handleX a = case a of
            Xr -> Ir
            _ -> a
          isParam p = case p of
            P _ -> True
            _   -> False  

rateParam :: Prim -> E
rateParam p = withRate Ir (ExpPrim p)

rateStringParam :: E -> E
rateStringParam p = setRate Sr $ strget p
    where strget :: E -> E
          strget = opc1 "strget" [(Sr, [Ir])]

rateTfm :: KrateSet -> Rate -> Info -> [E] -> E
rateTfm krateSet rootRate info as = withRate rootRate $ 
    Tfm info $ zipWith (rate krateSet) signature as   
    where signature = case infoSignature info of
            SingleRate a -> a M.! rootRate
            MultiRate _ a -> a

-- clearRate :: Exp E -> E
-- clearRate = Fix . RatedExp Nothing 

defaultRate :: Rate -> KrateSet -> Name -> Rate
defaultRate rootRate krateSet name 
    | isKrate krateSet name = Kr
    | otherwise = handleX $ rootRate
    where isKrate k n = S.member n k
          handleX a = case a of
            Xr -> Ar
            _ -> a  
    

possibleRate :: Rate -> Signature -> Rate
possibleRate desiredRate (SingleRate signature) = 
    fromJust $ find (flip M.member signature) (ratesFrom desiredRate)
    where ratesFrom x = case x of
            Ir -> [Ir, Kr, Ar]
            Kr -> [Kr, Ir, Ar]
            Ar -> [Ar, Kr, Ir]
possibleRate desiredRate (MultiRate _ _) = desiredRate            

convert :: Rate -> Rate -> E -> E
convert to from e 
    | coherent to from = e
    | otherwise = convertNotCoherent to from e
    where coherent to from = case (to, from) of
             (Xr, a)             -> True             
             (a, b)  | a == b    -> True
             (Kr, Ir)            -> True
             _                   -> False
           
    
    
convertNotCoherent :: Rate -> Rate -> E -> E
convertNotCoherent to from = 
    foldr (.) id $ map (uncurry insertConverter) $ adjacentPairs $ range to from
    where insertConverter :: Rate -> Rate -> E -> E
          insertConverter to from =
            withRate to . ConvertRate to from
    
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs xs = case xs of
    a:[]        -> []
    (a:b:as)    -> (a, b) : adjacentPairs (b:as)
    
range :: (Enum a, Ord a) => a  -> a -> [a]
range a b 
    | a <= b    = [a .. b]
    | otherwise = reverse [b .. a]

   


