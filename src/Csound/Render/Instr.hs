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

import Text.PrettyPrint hiding ((<>), render)
import qualified Text.PrettyPrint as P

import Csound.Tfm.DAG 
import Csound.Tfm.BiMap
import Csound.Exp
import Csound.Exp.Wrapper hiding (double, int, var)

import Csound.Tfm.RateGraph
import Csound.Tfm.TfmTree
import Csound.Exp.BoolExp(renderCondInfo)
import Csound.Exp.NumExp(renderNumExp)
import Csound.Exp.Inline

type InstrId = Int

renderInstr :: KrateSet -> TabMap -> InstrId -> E -> Doc
renderInstr krateSet ft instrId exp = instrHeader instrId $ renderInstrBody krateSet ft exp

instrHeader :: InstrId -> Doc -> Doc
instrHeader instrId body = vcat [
    text "instr" <+> int instrId,
    body ,
    text "endin"]
    

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
        
renderInstrBody :: KrateSet -> TabMap -> E -> Doc
renderInstrBody krateSet ft sig = vcat $ map (stmt . clearEmptyResults) $ collectRates krateSet st g
    where stmt :: ([RatedVar], Exp RatedVar) -> Doc
          stmt (res, exp) = args res <+> renderExp exp
          
          st = getRenderState g
          g  = toDag ft sig
 
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

var :: RatedVar -> Doc
var (RatedVar r x) = renderRate r P.<> int x

args :: [RatedVar] -> Doc
args xs = hsep $ punctuate comma $ map var xs

renderRate :: Rate -> Doc
renderRate x = case x of
    Sr -> char 'S'
    _  -> phi x
    where phi = text . map toLower . show 

assign :: Doc -> Doc
assign x = char '=' <+> x

renderExp :: Exp RatedVar -> Doc
renderExp x = case x of
    ExpPrim (PString n) -> text "strget" <+> char 'p' P.<> int n
    ExpPrim p -> assign $ renderPrim p
    Tfm info [a, b] | isInfix  info -> assign $ var a <+> text (infoName info) <+> var b
    Tfm info xs     -> text (infoName info) <+> args xs
    ConvertRate to from x -> renderConvertRate to from $ var x
    If info t e -> assign $ renderCondInfo var info <+> char '?' <+> var t <+> char ':' <+> var e
    ExpNum a -> assign $ renderNumExp var a
    WriteVar v a -> renderVar v <+> equals <+> var a
    ReadVar v -> assign $ renderVar v
    x -> error $ "unknown expression: " ++ show x
       

renderConvertRate :: Rate -> Rate -> Doc -> Doc
renderConvertRate to from var = case (to, from) of
    (Ar, Kr) -> upsamp var 
    (Ar, Ir) -> upsamp $ k var
    (Kr, Ar) -> downsamp var
    (Kr, Ir) -> equals <+> k var
    (Ir, Ar) -> downsamp var
    (Ir, Kr) -> equals <+> i var
    where upsamp x = text "upsamp" <+> x
          downsamp x = text "downsamp" <+> x
          k x = char 'k' P.<> parens x
          i x = char 'i' P.<> parens x


renderVar :: Var -> Doc
renderVar v = case v of
    Var ty rate name -> renderVarType ty P.<> renderRate rate P.<> text name
    VarVerbatim _ name -> text name

renderVarType :: VarType -> Doc
renderVarType x = case x of
    LocalVar -> P.empty
    GlobalVar -> char 'g'

renderPrim :: Prim -> Doc
renderPrim x = case x of
    P n -> char 'p' P.<> int n
    PrimInt n -> int n
    PrimDouble d -> double d
    PrimString s -> text s
    PrimTab f -> renderTab f
    
renderTab :: Tab -> Doc
renderTab (Tab size n xs) = text "gen" P.<> int n <+> int size <+> (hsep $ map double xs)
 
    
