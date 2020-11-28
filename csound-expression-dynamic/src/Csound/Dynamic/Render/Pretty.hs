module Csound.Dynamic.Render.Pretty(
    Doc, vcatSep,
    ppCsdFile, ppGen, ppNotes, ppInstr, ppStmt, ppTotalDur
) where

import Control.Monad.Trans.State.Strict
import Data.Char(toLower)
import qualified Data.IntMap as IM

import Text.PrettyPrint.Leijen
import Csound.Dynamic.Types
import qualified Csound.Dynamic.Tfm.DeduceTypes as R(Var(..))

vcatSep :: [Doc] -> Doc
vcatSep = vcat . punctuate line

binaries, unaries :: String -> [Doc] -> Doc

binaries op as = binary op (as !! 0) (as !! 1)
unaries  op as = unary  op (as !! 0)

binary :: String -> Doc -> Doc -> Doc
binary op a b = parens $ a <+> text op <+> b

unary :: String -> Doc -> Doc
unary op a = parens $ text op <> a

func :: String -> Doc -> Doc
func op a = text op <> parens a

ppCsdFile :: Doc -> Doc -> Doc -> [Plugin] -> Doc
ppCsdFile flags orc sco plugins = 
    tag "CsoundSynthesizer" $ vcatSep [
        tag "CsOptions" flags,
        tag "CsInstruments" orc,
        tag "CsScore" sco,
        ppPlugins plugins
        ]   

ppPlugins :: [Plugin] -> Doc
ppPlugins plugins = vcatSep $ fmap (\(Plugin name body) -> tag name (text body)) plugins

tag :: String -> Doc -> Doc
tag name content = vcatSep [
    char '<' <> text name <> char '>', 
    content, 
    text "</" <> text name <> char '>']  

ppNotes :: InstrId -> [CsdEvent] -> Doc
ppNotes instrId = vcat . fmap (ppNote instrId)

ppNote :: InstrId -> CsdEvent -> Doc
ppNote instrId evt = char 'i' 
    <+> ppInstrId instrId 
    <+> double (csdEventStart evt) <+> double (csdEventDur evt) 
    <+> hsep (fmap ppPrim $ csdEventContent evt)

ppPrim :: Prim -> Doc
ppPrim x = case x of
    P n -> char 'p' <> int n
    PrimInstrId a -> ppInstrId a
    PString a -> int a    
    PrimInt n -> int n
    PrimDouble d -> double d
    PrimString s -> dquotes $ text s
    PrimVar targetRate v -> ppConverter targetRate (varRate v) $ ppVar v
    where
        ppConverter dst src t 
            | dst == src = t            
            | dst == Ar && src == Kr = a(t) 
            | dst == Ar && src == Ir = a(k(t))            
            | dst == Kr  = k(t)
            | dst == Ir && src == Kr = i(t)
            | dst == Ir && src == Ar = i(k(t))
            | otherwise = t
            where 
                tfm ch v = hcat [char ch, parens v]    
                a = tfm 'a'
                k = tfm 'k'
                i = tfm 'i'

    
ppGen :: Int -> Gen -> Doc
ppGen tabId ft = char 'f' 
    <>  int tabId 
    <+> int 0 
    <+> (int $ genSize ft)
    <+> (ppGenId $ genId ft) 
    <+> (maybe empty (text . show) $ genFile ft)
    <+> (hsep $ map double $ genArgs ft)

ppGenId :: GenId -> Doc
ppGenId genId = case genId of
    IntGenId a      -> int a
    StringGenId a   -> dquotes $ text a

ppInstr :: InstrId -> Doc -> Doc
ppInstr instrId body = vcat [
    text "instr" <+> ppInstrHeadId instrId,
    body,
    text "endin"]

ppInstrHeadId :: InstrId -> Doc
ppInstrHeadId x = case x of
    InstrId den nom -> int nom <> maybe empty ppAfterDot den 
    InstrLabel name -> text name
    where ppAfterDot a = text $ ('.': ) $ reverse $ show a

ppInstrId :: InstrId -> Doc
ppInstrId x = case x of
    InstrId den nom -> int nom <> maybe empty ppAfterDot den 
    InstrLabel name -> dquotes $ text name
    where ppAfterDot a = text $ ('.': ) $ reverse $ show a

type TabDepth = Int

ppStmt :: [RatedVar] -> Exp RatedVar -> State TabDepth Doc
ppStmt outs expr = maybe (ppExp (ppOuts outs) expr) id (maybeStringCopy outs expr) 

maybeStringCopy :: [RatedVar] -> Exp RatedVar -> Maybe (State TabDepth Doc)
maybeStringCopy outs expr = case (outs, expr) of
    ([R.Var n Sr], ExpPrim (PrimVar rate var)) -> Just $ tab $ ppStringCopy (ppOuts outs) (ppVar var)
    ([R.Var n Sr], ReadVar var) -> Just $ tab $ ppStringCopy (ppOuts outs) (ppVar var)
    ([], WriteVar outVar a) | varRate outVar == Sr  -> Just $ tab $ ppStringCopy (ppVar outVar) (ppPrimOrVar a)
    ([R.Var n Sr], ReadArr var as) -> Just $ tab $ ppStringCopy (ppOuts outs) (ppReadArr var $ fmap ppPrimOrVar as)
    ([], WriteArr outVar bs a) | varRate outVar == Sr -> Just $ tab $ ppStringCopy (ppArrIndex outVar $ fmap ppPrimOrVar bs) (ppPrimOrVar a)
    _ -> Nothing

ppStringCopy :: Doc -> Doc -> Doc
ppStringCopy outs src = ppOpc outs "strcpyk" [src]

ppExp :: Doc -> Exp RatedVar -> State TabDepth Doc
ppExp res expr = case fmap ppPrimOrVar expr of
    ExpPrim (PString n)             -> tab $ ppStrget res n
    ExpPrim p                       -> tab $ res $= ppPrim p
    Tfm info [a, b] | isInfix  info -> tab $ res $= binary (infoName info) a b
    Tfm info xs     | isPrefix info -> tab $ res $= prefix (infoName info) xs
    Tfm info xs                     -> tab $ ppOpc res (infoName info) xs
    ConvertRate to from x           -> tab $ ppConvertRate res to from x
    If info t e                     -> tab $ ppIf res (ppCond info) t e
    ExpNum (PreInline op as)        -> tab $ res $= ppNumOp op as
    WriteVar v a                    -> tab $ ppVar v $= a
    InitVar v a                     -> tab $ ppOpc (ppVar v) "init" [a]
    ReadVar v                       -> tab $ res $= ppVar v

    InitArr v as                    -> tab $ ppOpc (ppArrVar (length as) v) "init" as
    ReadArr v as                    -> tab $ if (varRate v /= Sr) then res $= ppReadArr v as else res <+> text "strcpy" <+> ppReadArr v as 
    WriteArr v as b                 -> tab $ ppWriteArr v as b
    WriteInitArr v as b             -> tab $ ppWriteInitArr v as b
    TfmArr isInit v op [a,b]| isInfix  op  -> tab $ ppTfmArrOut isInit v <+> binary (infoName op) a b
    TfmArr isInit v op args | isPrefix op  -> tab $ ppTfmArrOut isInit v <+> prefix (infoName op) args
    TfmArr isInit v op xs                  -> tab $ ppOpc (ppTfmArrOut isInit v) (infoName op) xs

    IfBegin _ a                     -> succTab          $ text "if "     <> ppCond a <> text " then"
--     ElseIfBegin a                   -> left >> (succTab $ text "elseif " <> ppCond a <> text " then")    
    ElseBegin                       -> left >> (succTab $ text "else")
    IfEnd                           -> left >> (tab     $ text "endif")
    UntilBegin a                    -> succTab          $ text "until " <> ppCond a <> text " do"
    UntilEnd                        -> left >> (tab     $ text "od")
    WhileBegin a                    -> succTab          $ text "while " <> ppCond a <> text " do"
    WhileRefBegin var               -> succTab          $ text "while " <> ppVar var <+> equals <+> text "1" <+> text "do"
    WhileEnd                        -> left >> (tab     $ text "od")
    InitMacrosString name initValue -> tab $ initMacros (text name) (text initValue)
    InitMacrosDouble name initValue -> tab $ initMacros (text name) (double initValue)
    InitMacrosInt name initValue    -> tab $ initMacros (text name) (int initValue)
    ReadMacrosString name           -> tab $ res <+> text "strcpy" <+> readMacro name
    ReadMacrosDouble name           -> tab $ res $= readMacro name
    ReadMacrosInt name              -> tab $ res $= readMacro name
    EmptyExp                        -> return empty    
    Verbatim str                    -> return $ text str
    x -> error $ "unknown expression: " ++ show x


-- pp macros
 
readMacro name = char '$' <> text name

initMacros name initValue = vcat 
    [ text "#ifndef" <+> name
    , text "#define " <+> name <+> char '#' <> initValue <> char '#'
    , text "#end"
    ]

-- pp arrays

ppTfmArrOut isInit v = ppVar v <> (if isInit then (text "[]") else empty)

ppArrIndex v as = ppVar v <> (hcat $ fmap brackets as)
ppArrVar n v = ppVar v <> (hcat $ replicate n $ text "[]")

ppReadArr v as = ppArrIndex v as

ppWriteArr v as b = ppArrIndex v as <+> equalsWord <+> b
    where equalsWord = if (varRate v == Sr) then text "strcpy" else equals

ppWriteInitArr v as b = ppArrIndex v as <+> initWord <+> b
    where initWord = text $ if (varRate v == Sr) then "strcpy" else "init"

-------------------------------------

tab doc = fmap (shiftByTab doc) get 
tabWidth = 4
shiftByTab doc n
    | n == 0    = doc
    | otherwise = (text $ replicate (tabWidth * n) ' ') <> doc 

left = modify pred

succTab doc = do
    a <- tab doc
    modify succ
    return a

prefix name args = text name <> tupled args

ppCond :: Inline CondOp Doc -> Doc
ppCond = ppInline ppCondOp 

($=) :: Doc -> Doc -> Doc
($=) a b = a <+> equals <+> b

ppOuts :: [RatedVar] -> Doc
ppOuts xs = hsep $ punctuate comma $ map ppRatedVar xs

ppPrimOrVar :: PrimOr RatedVar -> Doc
ppPrimOrVar x = either ppPrim ppRatedVar $ unPrimOr x

ppStrget :: Doc -> Int -> Doc
ppStrget out n = ppOpc out "strget" [char 'p' <> int n]
 
ppIf :: Doc -> Doc -> Doc -> Doc -> Doc
ppIf res p t e = vcat 
    [ text "if" <+> p <+> text "then"
    , text "    " <> res <+> char '=' <+> t
    , text "else"
    , text "    " <> res <+> char '=' <+> e
    , text "endif" 
    ]

ppOpc :: Doc -> String -> [Doc] -> Doc
ppOpc out name xs = out <+> ppProc name xs

ppProc :: String -> [Doc] -> Doc
ppProc name xs = text name <+> (hsep $ punctuate comma xs)

ppVar :: Var -> Doc
ppVar v = case v of
    Var ty rate name   -> ppVarType ty <> ppRate rate <> text (varPrefix ty : name)
    VarVerbatim _ name -> text name

varPrefix :: VarType -> Char
varPrefix x = case x of
    LocalVar  -> 'l'
    GlobalVar -> 'g'

ppVarType :: VarType -> Doc
ppVarType x = case x of
    LocalVar  -> empty
    GlobalVar -> char 'g'

ppConvertRate :: Doc -> Rate -> Rate -> Doc -> Doc
ppConvertRate out to from var = case (to, from) of
    (Ar, Kr) -> upsamp var 
    (Ar, Ir) -> upsamp $ k var
    (Kr, Ar) -> downsamp var
    (Kr, Ir) -> out $= k var
    (Ir, Ar) -> downsamp var
    (Ir, Kr) -> out $= i var
    (a, b)   -> error $ "bug: no rate conversion from " ++ show b ++ " to " ++ show a ++ "."
    where 
        upsamp x = ppOpc out "upsamp" [x]
        downsamp x = ppOpc out "downsamp" [x]
        k = func "k"
        i = func "i"

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
    where 
        bi  = binaries
        uno = unaries

ppRatedVar :: RatedVar -> Doc
ppRatedVar v = ppRate (ratedVarRate v) <> int (ratedVarId v)

ppRate :: Rate -> Doc
ppRate x = case x of
    Sr -> char 'S'
    _  -> phi x
    where phi = text . map toLower . show 

ppTotalDur :: Double -> Doc
ppTotalDur d = text "f0" <+> double d

