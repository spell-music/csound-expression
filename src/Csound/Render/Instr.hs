module Csound.Render.Instr where

import qualified Data.IntMap as IM
-- import Control.Monad.State
import Data.Char(toLower)
import Data.List(partition, sortBy)
import Control.Arrow(second)
import Data.Ord(comparing)

import Text.PrettyPrint hiding ((<>), render)
import qualified Text.PrettyPrint as P

import Csound.Tfm.DAG 
import Csound.Tfm.BiMap
import Csound.Exp
import Csound.Exp.Wrapper hiding (double, int, var)

import Csound.Tfm.Rate
import Csound.Tfm.TfmTree
import Csound.Exp.BoolExp(renderCondInfo)


instance Show Sig where
    show a = show $ renderInstrBody OutPlain (ftableMap [exp]) exp
        where exp = unSig a


type InstrId = Int


renderInstr :: OutType -> FtableMap -> InstrId -> E -> Doc
renderInstr outType ft instrId exp = instrHeader instrId $ renderInstrBody outType ft exp

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

getRenderState :: Dag RatedExp -> (RenderState, Dag RatedExp)
getRenderState a = (RenderState moLinks moRates, rest)
    where moLinks = IM.fromListWith (++) $ map extract selectInfo
          moRates = fmap (second getRate) selectInfo           

          (selectInfo, rest) = partition (isSelect . snd) a
    
          isSelect x = case x of
            RatedExp _ (Select _ _) -> True
            _ -> False
            
          extract (n, (RatedExp _ (Select order parent))) = (parent, [MultiOutPort n order])


toDag :: FtableMap -> E -> Dag RatedExp 
toDag ft exp = dag $ substFtables ft $ rate defaultKrateSet Ar $ exp


clearEmptyResults :: ([RatedVar], Exp RatedVar) -> ([RatedVar], Exp RatedVar)
clearEmptyResults x@(res, exp)
    | isProcedure exp = ([], exp)
    | otherwise = x
    where isProcedure x = case x of
              Outs _            -> True
              ExpBuf Delayw _ _ -> True
              _                 -> False  
        
renderInstrBody :: OutType -> FtableMap -> E -> Doc
renderInstrBody outType ft sig = vcat $ map (stmt . clearEmptyResults) $ collectRates st g1
    where stmt :: ([RatedVar], Exp RatedVar) -> Doc
          stmt (res, exp) = args res <+> renderExp outType exp
          
          (st, g1) = getRenderState g0
          g0 = toDag ft sig
    
            

data RatedVar = RatedVar Rate String 
    deriving (Show)

collectRates :: RenderState -> Dag RatedExp -> [([RatedVar], Exp RatedVar)]
collectRates st a = map substRate a
    where rateMap = IM.fromList $ multiOutsRates st ++ fmap (second getRate) a
          substRate (n, exp) 
            | isMultiOutExp e = (getMultiOutVars n (multiOutsLinks st IM.! n) e, fmap makeVar e)
            | otherwise = ([makeVar n], fmap makeVar e)
            where e = getExp exp
                  r = getRate exp

          makeVar n = RatedVar (rateMap IM.! n) (show n)
          
isMultiOutExp :: Exp a -> Bool
isMultiOutExp x = case x of
    Tfm i _ -> isMultiOutSignature (infoSignature i)
    _ -> False
    
getMultiOutVars :: Int -> [MultiOutPort] -> Exp Int -> [RatedVar]
getMultiOutVars parent ports exp = zipWith RatedVar (getRates exp) (getPorts parent ports)
    where getPorts parentId ps = concat $ names ++ [map (missingPortName parent) [lastId .. ]]
            where ps' = sortBy (comparing orderMultiOutPort) ps
                  (names, lastId) = runState (mapM (fillMissingPorts parentId) ps') 0
                    

          missingPortName parentId n = show parentId ++ "_" ++ show n
            
          fillMissingPorts :: Int -> MultiOutPort -> State Int [String]
          fillMissingPorts parentId port = state $ \s ->
                if s == order
                then ([e], next) 
                else (map (missingPortName parentId) [s .. order - 1] ++ [e], next)
            where e = show $ idMultiOutPort port
                  order = orderMultiOutPort port                  
                  next = order + 1
            

getRate :: RatedExp a -> Rate
getRate (RatedExp (Just r) _) = r
getRate _ = error "getRate: rate is undefined"

getExp :: RatedExp a -> Exp a
getExp (RatedExp _ exp) = exp

          
var :: RatedVar -> Doc
var (RatedVar r x) = renderRate r P.<> text x

args :: [RatedVar] -> Doc
args xs = hsep $ punctuate comma $ map var xs

renderRate :: Rate -> Doc
renderRate x = case x of
    Sr -> char 'S'
    _  -> phi x
    where phi = text . map toLower . show 

assign :: Doc -> Doc
assign x = char '=' <+> x

renderExp :: OutType -> Exp RatedVar -> Doc
renderExp outType x = case x of
    ExpPrim p -> assign $ renderPrim p
    Tfm info xs     | isPrefix info -> text (infoName info) <+> args xs
    Tfm info [a, b] | isInfix  info -> assign $ var a <+> text (infoName info) <+> var b
    ConvertRate a b x -> assign $ var x
    If info t e -> equals <+> renderCondInfo var info <+> char '?' <+> var t <+> char ':' <+> var e
    Outs xs -> renderOuts outType xs     
    ExpBuf op _ a -> (renderBufOp op) <+> var a         
    Depends _ a -> equals <+> var a
    Var ty rate name -> equals <+> renderVarType ty P.<> renderRate rate P.<> text "var_" P.<> text name
    x -> error $ "unknown expression: " ++ show x


renderOuts :: OutType -> [RatedVar] -> Doc
renderOuts ty xs = case ty of
    OutPlain -> outPlain xs
    OutInstrPort n -> vcat $ zipWith portUpdate (fmap (gOut n) [1 .. length xs]) xs
    OutFile fileName -> fout fileName xs $$ outPlain xs    
    where outPlain xs = outName xs <+> args xs 
          outName xs = text $ if (length xs == 1) then "out" else "outs"
          portUpdate port x = port <+> equals <+> port <+> char '+' <+> var x
          fout fileName xs = text "fout" <+> text fileName P.<> text ", 15," <+> args xs             
           

gOut :: Int -> Int -> Doc
gOut instrId portId = text "gaOut" P.<> int instrId P.<> char '_' P.<> int portId

gOutVar :: Val a => Int -> Int -> a
gOutVar instrId portId = gvar Ar $ gOutName instrId portId

gOutName :: Int -> Int -> String
gOutName instrId portId = show $ gOut instrId portId 

renderVarType :: VarType -> Doc
renderVarType x = case x of
    LocalVar -> P.empty
    GlobalVar -> char 'g'

renderBufOp x = text $ case x of
    Delayr -> "delayr"
    Delayw -> "delayw"
    Deltap -> "deltap"

renderPrim :: Prim -> Doc
renderPrim x = case x of
    P n -> char 'p' P.<> int n
    PrimInt n -> int n
    PrimDouble d -> double d
    PrimString s -> text s
    PrimFtable f -> renderFtable f
    
renderFtable :: Ftable -> Doc
renderFtable (Ftable size n xs) = text "gen" P.<> int n <+> int size <+> (hsep $ map double xs)
 
    
