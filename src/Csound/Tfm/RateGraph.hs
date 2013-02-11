module Csound.Tfm.RateGraph(
    grate, KrateSet
) where

import Data.List(sort, sortBy, nub, find)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Data.Ord(comparing)
import Data.Maybe(fromJust)

import Data.Default

import Data.STRef
import Control.Monad.ST

import Data.Array
import Data.Array.ST
import Data.Array.MArray

import Csound.Exp

import Debug.Trace

echo :: Show a => String -> a -> a
echo msg a = trace (msg ++ ": " ++ show a) a

type AgentId = Int

data Agent = Agent 
    { agentResponses :: [Response]
    , agentQueries :: [Query]
    , agentRate :: Rate
    , agentConversions :: [Conversion] }

type Conversion = (RatedVar, Exp RatedVar)

type KrateSet = S.Set Name

instance Default Agent where
    def = Agent [] [] Xr []

data Addr = Addr
    { addrLine :: Int
    , addrArg  :: Int 
    } deriving (Show)

data Query = Query
    { queryAddr :: Addr
    , queryRate :: Rate 
    } deriving (Show)

data Response  = Response
    { responseAddr      :: Addr    
    , responseRatedVar  :: RatedVar 
    } deriving (Show)
     

newtype MsgBox s = MsgBox { unMsgBox :: STArray s Int Agent }

modifyArray :: Ix i => STArray s i a -> i -> (a -> a) -> ST s ()
modifyArray arr i f = writeArray arr i . f =<< readArray arr i

msgBox :: Int -> ST s (MsgBox s)
msgBox size = fmap MsgBox $ newArray (0, size - 1) def

sendQuery :: AgentId -> Query -> MsgBox s -> ST s ()
sendQuery pid q box = modifyArray (unMsgBox box) pid $ 
    \x -> x{ agentQueries = q : agentQueries x }

sendResponse :: AgentId -> Response -> MsgBox s -> ST s ()
sendResponse pid r box = modifyArray (unMsgBox box) pid $ 
    \x -> x{ agentResponses = r : agentResponses x }

loadAgent :: AgentId -> MsgBox s -> ST s Agent
loadAgent pid box = readArray (unMsgBox box) pid

saveAgent :: AgentId -> MsgBox s -> Agent -> ST s ()
saveAgent pid box e = writeArray (unMsgBox box) pid e

discussLine :: KrateSet -> MsgBox s -> STRef s Int -> (Int, RatedExp Int) -> ST s ()
discussLine krateSet box freshIds (pid, exp) = do
    ag <- loadAgent pid box
    let desiredRates = nub $ map queryRate $ agentQueries ag
        curRate = deduceRate krateSet desiredRates exp
    notifyChildren pid curRate (ratedExpExp exp) box
    convTab <- conversionTable freshIds pid curRate desiredRates
    notifyParents box convTab (agentQueries ag)
    saveAgent pid box $ ag{ agentRate = curRate, agentConversions = getConversions pid curRate convTab }
        
deduceRate :: KrateSet -> [Rate] -> RatedExp Int -> Rate
deduceRate krateSet desiredRates exp = case ratedExpExp exp of
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
              [Xr] -> let newDesiredRates = if S.member name krateSet then [Kr] else [Ar]
                      in  tfmNoRate name newDesiredRates tab                
              Xr:as -> tfmNoRate name as tab
              as -> fromJust $ find (flip M.member tab) (as ++ [minBound .. maxBound])         
   

notifyChildren :: AgentId -> Rate -> ExpOr Int -> MsgBox s -> ST s ()
notifyChildren pid curRate exp box = mapM_ (\(to, query) -> sendQuery to query box) $ case exp of
    Tfm info xs -> notifyTfm curRate (infoSignature info) xs
    WriteVar v a -> [(a, mkQuery 0 $ varRate v)]
    If info a b -> (a, mkQuery (-2) curRate) : (b, mkQuery (-1) curRate) : encodeIfEnv (max Kr curRate) (inlineEnv info)
    ExpNum (PreInline op xs) -> queryList xs (repeat curRate)
    _ -> []
    where notifyTfm r signature xs = case signature of
            SingleRate table -> queryList xs $ table M.! r
            MultiRate _ rs -> queryList xs rs

          queryList args rates = zipWith3 (\n a r -> (a, mkQuery n r)) [0 .. ] args rates

          mkQuery n r = Query (Addr pid n) r

          encodeIfEnv rate info = map (\(port, arg) -> (arg, mkQuery port rate)) $ IM.toList info 

notifyParents :: MsgBox s -> M.Map Rate RatedVar -> [Query] -> ST s ()
notifyParents box convTab qs = mapM_ (notifyParent box convTab) qs

notifyParent :: MsgBox s -> M.Map Rate RatedVar -> Query -> ST s ()
notifyParent box convTab q = sendResponse (addrLine $ queryAddr q) (Response (queryAddr q) (convTab M.! queryRate q)) box


conversionTable :: STRef s Int -> Int -> Rate -> [Rate] -> ST s (M.Map Rate RatedVar)
conversionTable freshIds curId curRate desiredRates = fmap M.fromList $ mapM (flip mkRatedVar curRate) desiredRates
    where coherentRates to from = case (to, from) of
              (Xr, a)             -> True             
              (a, b)  | a == b    -> True
              (Kr, Ir)            -> True
              _                   -> False

          mkRatedVar to from 
              | coherentRates to from = return $ (to, RatedVar curRate curId)
              | otherwise = do 
                  n <- newId freshIds       
                  return $ (to, RatedVar to n)

          newId x = do 
             n <- readSTRef x   
             modifySTRef x succ
             return n      

getConversions :: AgentId -> Rate -> M.Map Rate RatedVar -> [Conversion]
getConversions pid curRate convTable = uncurry phi =<< M.toList convTable
    where phi rate var@(RatedVar r n)
            | n == pid  = []
            | otherwise = [(var, ConvertRate r curRate $ RatedVar curRate pid)] 


processLine :: MsgBox s -> (Int, RatedExp Int) -> ST s [(RatedVar, Exp RatedVar)]
processLine box (pid, exp) = fmap phi $ loadAgent pid box     
    where phi a = agentConversions a 
            ++ return (RatedVar (agentRate a) pid, rateExp (agentRate a) (agentResponses a) (ratedExpExp exp)) 


rateExp :: Rate -> [Response] -> Exp Int -> Exp RatedVar 
rateExp curRate rs exp = case exp of
    ExpPrim (P n) | curRate == Sr -> ExpPrim (PString n)
    ExpPrim p -> ExpPrim p
    Tfm i _ -> Tfm i vs   
    Select rate pid a -> Select rate pid (RatedVar Xr a)    
    If condInfo _ _ -> case vs of
        a:b:rest -> If (decodeIfEnv condInfo rest) a b
    ExpNum (PreInline op _) -> ExpNum (PreInline op vs)
    ReadVar v -> ReadVar v
    WriteVar v _ -> WriteVar v (head vs)
    where vs = map responseRatedVar $ sortBy (comparing $ addrArg . responseAddr) rs
          decodeIfEnv info xs = info{ inlineEnv = IM.fromList $ zip [0..] xs }


findRate :: [Rate] -> Rate
findRate [x] = x
findRate xs = case sort $ nub xs of
    [a] -> a
    [] -> Xr
    Xr:as -> minimum as
    as -> minimum as
        

grate :: KrateSet -> [(Int, RatedExp Int)] -> ([(RatedVar, Exp RatedVar)], Int)
grate krateSet as = runST $ do
    freshIds <- newSTRef n
    box <- msgBox n    
    mapM_ (discussLine krateSet box freshIds) lines
    graph <- fmap (reverse . concat) $ mapM (processLine box) lines
    lastFreshId <- readSTRef freshIds
    return (graph, lastFreshId)
    where n = length as
          lines = reverse as 
  



