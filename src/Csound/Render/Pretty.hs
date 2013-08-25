module Csound.Render.Pretty (
    Doc, int, double, text, empty, ($$), hcat, vcat, vcatMap, clearSpace, (<+>),
    punctuate, comma,
    verbatimLines,

    binaries, unaries, funcs,
    binary, unary, func,
    ppMapTable,
    ppStmt,
    ($=), ppOpc, ppMoOpc, ppProc, ppVar,
    ppPrim, ppTab, ppStrget, ppStrset, ppTabDef, ppConvertRate, ppIf,
    ppCsdFile, ppInstr, ppInstr0, ppScore, ppNote, ppTotalDur, ppOrc, ppSco, 
    ppInline, ppCondOp, ppNumOp,
    ppEvent, ppMasterNote, ppAlwayson
) where

import Control.Monad.Trans.State.Strict
import Data.Char(toLower)
import qualified Data.Map as M
import qualified Data.IntMap as IM

import Text.PrettyPrint.Leijen

import Csound.Tfm.Tab
import Csound.Exp 
import Csound.Exp.EventList

vcatMap :: (a -> Doc) -> [a] -> Doc
vcatMap f = vcat . fmap f

verbatimLines :: [String] -> Doc
verbatimLines = vcat . fmap text

($$) :: Doc -> Doc -> Doc
($$) = (<$$>)

binaries, unaries, funcs :: String -> [Doc] -> Doc

binaries op as = binary op (as !! 0) (as !! 1)
unaries  op as = unary  op (as !! 0)
funcs    op as = func   op (as !! 0)

binary :: String -> Doc -> Doc -> Doc
binary op a b = parens $ a <+> text op <+> b

unary :: String -> Doc -> Doc
unary op a = parens $ text op <> a

func :: String -> Doc -> Doc
func op a = text op <> parens a

ppMapTable :: (a -> Int -> Doc) -> Index a -> Doc
ppMapTable phi = vcat . map (uncurry phi) . M.toList . indexElems


ppRate :: Rate -> Doc
ppRate x = case x of
    Sr -> char 'S'
    _  -> phi x
    where phi = text . map toLower . show 

ppPrimOrVar :: PrimOr RatedVar -> Doc
ppPrimOrVar x = either ppPrim ppRatedVar $ unPrimOr x

ppRatedVar :: RatedVar -> Doc
ppRatedVar v = ppRate (ratedVarRate v) <> int (ratedVarId v)

ppOuts :: [RatedVar] -> Doc
ppOuts xs = hsep $ punctuate comma $ map ppRatedVar xs

($=) :: Doc -> Doc -> Doc
($=) a b = a <+> equals <+> b

ppOpc :: Doc -> String -> [Doc] -> Doc
ppOpc out name xs = out <+> ppProc name xs

ppMoOpc :: [Doc] -> String -> [Doc] -> Doc
ppMoOpc outs = ppOpc (hcat $ punctuate comma outs)

ppProc :: String -> [Doc] -> Doc
ppProc name xs = text name <+> (hsep $ punctuate comma xs)

ppVar :: Var -> Doc
ppVar v = case v of
    Var ty rate name -> ppVarType ty <> ppRate rate <> text (varPrefix ty : name)
    VarVerbatim _ name -> text name

varPrefix :: VarType -> Char
varPrefix x = case x of
    LocalVar  -> 'l'
    GlobalVar -> 'g'

ppVarType :: VarType -> Doc
ppVarType x = case x of
    LocalVar  -> empty
    GlobalVar -> char 'g'

ppPrim :: Prim -> Doc
ppPrim x = case x of
    P n -> char 'p' <> int n
    PrimInstrId a -> ppInstrId a
    PString a -> int a    
    PrimInt n -> int n
    PrimDouble d -> double d
    PrimString s -> dquotes $ text s
    PrimTab f -> error $ "i'm lost table, please substitute me (" ++ (show f) ++ ")" 
    
ppTab :: LowTab -> Doc
ppTab (LowTab size n xs mfile) = text "gen" <> int n <+> int size <+> file <+> (hsep $ map double xs)
    where file = maybe empty (text . show) mfile
 
ppIf :: Doc -> Doc -> Doc -> Doc
ppIf p t e = p <+> char '?' <+> t <+> char ':' <+> e

ppStrget :: Doc -> Int -> Doc
ppStrget out n = ppOpc out "strget" [char 'p' <> int n]

ppConvertRate :: Doc -> Rate -> Rate -> Doc -> Doc
ppConvertRate out to from var = case (to, from) of
    (Ar, Kr) -> upsamp var 
    (Ar, Ir) -> upsamp $ k var
    (Kr, Ar) -> downsamp var
    (Kr, Ir) -> out $= k var
    (Ir, Ar) -> downsamp var
    (Ir, Kr) -> out $= i var
    (a, b)   -> error $ "bug: no rate conversion from " ++ show b ++ " to " ++ show a ++ "."
    where upsamp x = ppOpc out "upsamp" [x]
          downsamp x = ppOpc out "downsamp" [x]
          k = func "k"
          i = func "i"

ppTabDef :: LowTab -> Int -> Doc
ppTabDef ft tabId = char 'f' 
    <>  int tabId 
    <+> int 0 
    <+> (int $ lowTabSize ft)
    <+> (int $ lowTabGen ft) 
    <+> (maybe empty (text . show) $ lowTabFile ft)
    <+> (hsep $ map double $ lowTabArgs ft)

ppStrset :: String -> Int -> Doc
ppStrset str strId = text "strset" <+> int strId <> comma <+> (dquotes $ text str)

-- file

newline :: Doc
newline = line

ppCsdFile :: Doc -> Doc -> Doc -> Doc -> Doc -> Doc -> Doc
ppCsdFile flags instr0 instrs scores strTable tabs = 
    tag "CsoundSynthesizer" [
        tag "CsOptions" [flags],
        tag "CsInstruments" [
            instr0, strTable, instrs],
        tag "CsScore" [
            tabs, scores]]    

tag :: String -> [Doc] -> Doc
tag name content = vcat $ punctuate newline [
    char '<' <> text name <> char '>', 
    vcat $ punctuate newline content, 
    text "</" <> text name <> char '>']  

-- instrument

ppInstr :: InstrId -> Doc -> Doc
ppInstr instrId body = vcat [
    newline,
    text "instr" <+> ppInstrId instrId,
    body,
    text "endin"]

ppInstr0 :: [Doc] -> Doc
ppInstr0 = vcat . punctuate newline

ppOrc :: [Doc] -> Doc
ppOrc = vcat . punctuate newline 

ppInstrId :: InstrId -> Doc
ppInstrId (InstrId den nom) = int nom <> maybe empty ppAfterDot den 
    where ppAfterDot x = text $ ('.': ) $ reverse $ show x

-- score

ppSco :: [Doc] -> Doc
ppSco = vcat

ppScore :: [Doc] -> Doc
ppScore = vcat

ppNote :: InstrId -> Double -> Double -> [Doc] -> Doc
ppNote instrId time dur args = char 'i' <> ppInstrId instrId <+> double time <+> double dur <+> hsep args

ppMasterNote :: InstrId -> CsdEvent [Prim] -> Doc
ppMasterNote instrId evt = ppNote instrId (csdEventStart evt) (csdEventDur evt) (fmap ppPrim $ csdEventContent evt) <+> int 0

ppEvent :: InstrId -> CsdEvent [Prim] -> Var -> Doc
ppEvent instrId evt var = pre <> comma <+> ppVar var
    where pre = ppProc "event_i" $ dquotes (char 'i') : ppInstrId instrId 
                : (double $ csdEventStart evt) : (double $ csdEventDur evt) : (fmap ppPrim $ csdEventContent evt)

ppTotalDur :: Double -> Doc
ppTotalDur d = text "f0" <+> double d

ppAlwayson :: InstrId -> Doc
ppAlwayson instrId = char 'i' <> ppInstrId instrId <+> int 0  <+> int (-1)

-- expressions

ppInline :: (a -> [Doc] -> Doc) -> Inline a Doc -> Doc
ppInline ppNode a = iter $ inlineExp a    
    where iter x = case x of
              InlinePrim n        -> inlineEnv a IM.! n
              InlineExp op args   -> ppNode op $ fmap iter args  

-- booleans

ppCondOp :: CondOp -> [Doc] -> Doc  
ppCondOp op = case op of
    TrueOp            -> const $ text "(1 == 1)"                
    FalseOp           -> const $ text "(0 == 1)"
    And               -> bi "&&"
    Or                -> bi "||"
    Equals            -> bi "=="
    NotEquals         -> bi "!="
    Less              -> bi "<"
    Greater           -> bi ">"
    LessEquals        -> bi "<="    
    GreaterEquals     -> bi ">="                         
    where bi  = binaries 
          
-- numeric

ppNumOp :: NumOp -> [Doc] -> Doc
ppNumOp op = case  op of
    Add -> bi "+"
    Sub -> bi "-"
    Mul -> bi "*"
    Div -> bi "/"
    Neg -> uno "-"    
    Pow -> bi "^"    
    Mod -> bi "%"
    
    ExpOp -> fun "exp"
    IntOp -> fun "int" 
    
    x -> fun (firstLetterToLower $ show x)        
    where bi  = binaries
          uno = unaries
          fun = funcs
          firstLetterToLower xs = case xs of
            a:as -> toLower a : as
            [] -> error "ppNumOp firstLetterToLower: empty identifier"


type TabDepth = Int

ppStmt :: [RatedVar] -> Exp RatedVar -> State TabDepth Doc
ppStmt outs expr = ppExp (ppOuts outs) expr

ppExp :: Doc -> Exp RatedVar -> State TabDepth Doc
ppExp res expr = case fmap ppPrimOrVar expr of
    ExpPrim (PString n)             -> tab $ ppStrget res n
    ExpPrim p                       -> tab $ res $= ppPrim p
    Tfm info [a, b] | isInfix  info -> tab $ res $= binary (infoName info) a b
    Tfm info xs                     -> tab $ ppOpc res (infoName info) xs
    ConvertRate to from x           -> tab $ ppConvertRate res to from x
    If info t e                     -> tab $ res $= ppIf (ppCond info) t e
    ExpNum (PreInline op as)        -> tab $ res $= ppNumOp op as
    WriteVar v a                    -> tab $ ppVar v $= a
    InitVar v a                     -> tab $ ppOpc (ppVar v) "init" [a]
    ReadVar v                       -> tab $ res $= ppVar v

    IfBegin a                       -> succTab          $ text "if "     <> ppCond a <> text " then"
    ElseIfBegin a                   -> left >> (succTab $ text "elseif " <> ppCond a <> text " then")    
    ElseBegin                       -> left >> (succTab $ text "else")
    IfEnd                           -> left >> (tab     $ text "endif")
    EmptyExp                        -> return empty
    x -> error $ "unknown expression: " ++ show x
    where tab doc = fmap (shiftByTab doc) get 
          tabWidth = 4
          shiftByTab doc n
            | n == 0    = doc
            | otherwise = (text $ replicate (tabWidth * n) ' ') <> doc 

          left = modify pred
          succTab doc = do
            a <- tab doc
            modify succ
            return a

ppCond :: Inline CondOp Doc -> Doc
ppCond = ppInline ppCondOp 
  
clearSpace :: Doc -> Doc
clearSpace x = newline $$ x $$ newline
