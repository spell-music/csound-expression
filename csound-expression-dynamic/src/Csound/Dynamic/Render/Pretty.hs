module Csound.Dynamic.Render.Pretty(
    Doc, vcatSep,
    ppCsdFile, ppGen, ppNotes, ppInstr, ppStmt, ppTotalDur,
    PrettyE(..), PrettyShowE(..),
    ppE
) where

import Control.Monad.Trans.State.Strict
import qualified Data.IntMap as IM

import Text.PrettyPrint.Leijen.Text
import Csound.Dynamic.Types
import Csound.Dynamic.Tfm.InferTypes qualified as R(Var(..))
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Show.Pretty (ppShow)
import Data.Fix (foldFix)
import Data.ByteString.Base64 qualified as Base64
import Data.Text.Encoding qualified as Text

vcatSep :: [Doc] -> Doc
vcatSep = vcat . punctuate line

binaries, unaries :: Text -> [Doc] -> Doc

binaries op as = binary op (as !! 0) (as !! 1)
unaries  op as = unary  op (as !! 0)

binary :: Text -> Doc -> Doc -> Doc
binary op a b = parens $ a <+> textStrict op <+> b

unary :: Text -> Doc -> Doc
unary op a = parens $ textStrict op <> a

func :: Text -> Doc -> Doc
func op a = textStrict op <> parens a

ppCsdFile :: Doc -> Doc -> Doc -> [Plugin] -> Doc
ppCsdFile flags orc sco plugins =
    tag "CsoundSynthesizer" $ vcatSep [
        tag "CsOptions" flags,
        tag "CsInstruments" orc,
        tag "CsScore" sco,
        ppPlugins plugins
        ]

ppPlugins :: [Plugin] -> Doc
ppPlugins plugins = vcatSep $ fmap (\(Plugin name body) -> tag name (textStrict body)) plugins

tag :: Text -> Doc -> Doc
tag name content = vcatSep [
    char '<' <> textStrict name <> char '>',
    content,
    text "</" <> textStrict name <> char '>']

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
    PrimString s -> dquotes $ textStrict s
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
    <+> (maybe empty (textStrict . Text.pack . show) $ genFile ft)
    <+> (hsep $ map double $ genArgs ft)

ppGenId :: GenId -> Doc
ppGenId x = case x of
    IntGenId a      -> int a
    StringGenId a   -> dquotes $ textStrict a

ppInstr :: InstrId -> Doc -> Doc
ppInstr instrId body = vcat [
    text "instr" <+> ppInstrHeadId instrId,
    body,
    text "endin"]

ppInstrHeadId :: InstrId -> Doc
ppInstrHeadId x = case x of
    InstrId den nom -> int nom <> maybe empty ppAfterDot den
    InstrLabel name -> textStrict name
    where ppAfterDot a = textStrict $ Text.pack $ ('.': ) $ reverse $ show a

ppInstrId :: InstrId -> Doc
ppInstrId x = case x of
    InstrId den nom -> int nom <> maybe empty ppAfterDot den
    InstrLabel name -> dquotes $ textStrict name
    where ppAfterDot a = textStrict $ Text.pack $ ('.': ) $ reverse $ show a

type TabDepth = Int

ppStmt :: [R.Var] -> Exp R.Var -> State TabDepth Doc
ppStmt outs expr = maybe (ppExp (ppOuts outs) expr) id (maybeStringCopy outs expr)

maybeStringCopy :: [R.Var] -> Exp R.Var -> Maybe (State TabDepth Doc)
maybeStringCopy outs expr = case (outs, expr) of
    ([R.Var Sr _], ExpPrim (PrimVar _rate var)) -> Just $ tab $ ppStringCopy (ppOuts outs) (ppVar var)
    ([R.Var Sr _], ReadVar var) -> Just $ tab $ ppStringCopy (ppOuts outs) (ppVar var)
    ([], WriteVar outVar a) | varRate outVar == Sr  -> Just $ tab $ ppStringCopy (ppVar outVar) (ppPrimOrVar a)
    ([R.Var Sr _], ReadArr var as) -> Just $ tab $ ppStringCopy (ppOuts outs) (ppReadArr var $ fmap ppPrimOrVar as)
    ([], WriteArr outVar bs a) | varRate outVar == Sr -> Just $ tab $ ppStringCopy (ppArrIndex outVar $ fmap ppPrimOrVar bs) (ppPrimOrVar a)
    _ -> Nothing

ppStringCopy :: Doc -> Doc -> Doc
ppStringCopy outs src = ppOpc outs "strcpyk" [src]

ppExp :: Doc -> Exp R.Var -> State TabDepth Doc
ppExp res expr = case fmap ppPrimOrVar expr of
    ExpPrim (PString n)             -> tab $ ppStrget res n
    ExpPrim p                       -> tab $ res $= ppPrim p
    Tfm info [a, b] | isInfix  info -> tab $ res $= binary (infoName info) a b
    Tfm info xs     | isPrefix info -> tab $ res $= prefix (infoName info) xs
    Tfm info xs                     -> tab $ ppOpc res (infoName info) xs
    ConvertRate to from x           -> tab $ ppConvertRate res to from x
    If _ifRate info t e             -> tab $ ppIf res (ppCond info) t e
    ExpNum (PreInline op as)        -> tab $ res $= ppNumOp op as
    WriteVar v a                    -> tab $ ppVar v $= a
    InitVar v a                     -> tab $ ppOpc (ppVar v) "init" [a]
    ReadVar v                       -> tab $ res $= ppVar v

    InitArr v as                    -> tab $ ppOpc (ppArrVar (length as) (ppVar v)) "init" as
    ReadArr v as                    -> tab $ if (varRate v /= Sr) then res $= ppReadArr v as else res <+> text "strcpy" <+> ppReadArr v as
    WriteArr v as b                 -> tab $ ppWriteArr v as b
    WriteInitArr v as b             -> tab $ ppWriteInitArr v as b
    TfmArr isInit v op [a,b]| isInfix  op  -> tab $ ppTfmArrOut isInit v <+> binary (infoName op) a b
    TfmArr isInit v op args | isPrefix op  -> tab $ ppTfmArrOut isInit v <+> prefix (infoName op) args
    TfmArr isInit v op xs                  -> tab $ ppOpc (ppTfmArrOut isInit v) (infoName op) xs

    InitPureArr _outRate _procRate initVals -> tab $ ppOpc (ppArrVar 1 res) "fillarray" initVals
    ReadPureArr outRate _procRate arr index -> tab $ if (outRate /= Sr) then res $= ppReadPureArr arr [index] else res <+> text "strcpy" <+> ppReadPureArr arr [index]

    IfBegin _ a                     -> succTab          $ text "if "     <> ppCond a <> text " then"
    IfBlock _ cond (CodeBlock th) ->  tab $ ppIf1 res (ppCond cond)  th
    IfElseBlock _ cond (CodeBlock th) (CodeBlock el) -> tab $ ppIf res (ppCond cond)  th el
--     ElseIfBegin a                   -> left >> (succTab $ text "elseif " <> ppCond a <> text " then")
    ElseBegin                       -> left >> (succTab $ text "else")
    IfEnd                           -> left >> (tab     $ text "endif")
    UntilBlock _ cond (CodeBlock th) -> tab $ ppUntil res (ppCond cond)  th
    WhileBlock _ cond (CodeBlock th) -> tab $ ppWhile res (ppCond cond)  th
    WhileRefBlock var (CodeBlock th) -> tab $ ppWhileRef res var th

    UntilBegin _ a                  -> succTab          $ text "until " <> ppCond a <> text " do"
    UntilEnd                        -> left >> (tab     $ text "od")
    WhileBegin _ a                  -> succTab          $ text "while " <> ppCond a <> text " do"
    WhileRefBegin var               -> succTab          $ text "while " <> ppVar var <+> equals <+> text "1" <+> text "do"
    WhileEnd                        -> left >> (tab     $ text "od")
    InitMacrosString name initValue -> tab $ initMacros (textStrict name) (textStrict initValue)
    InitMacrosDouble name initValue -> tab $ initMacros (textStrict name) (double initValue)
    InitMacrosInt name initValue    -> tab $ initMacros (textStrict name) (int initValue)
    ReadMacrosString name           -> tab $ res <+> text "strcpy" <+> readMacro name
    ReadMacrosDouble name           -> tab $ res $= readMacro name
    ReadMacrosInt name              -> tab $ res $= readMacro name
    EmptyExp                        -> return empty
    Verbatim str                    -> return $ textStrict str

    Select _rate _n a                 -> tab $ res $= ("SELECTS" <+> a)
    Starts                          -> tab $ res $= "STARTS"
    Seq a b                         -> tab $ hsep ["SEQ", a, b]
    Ends _a                          -> tab $ "ENDS"
    ExpBool _                        -> tab "ExpBool"

    -- x -> error $ "unknown expression: " ++ show x

-- pp macros

readMacro :: Text -> Doc
readMacro name = char '$' <> textStrict name

initMacros :: Doc -> Doc -> Doc
initMacros name initValue = vcat
    [ text "#ifndef" <+> name
    , text "#define " <+> name <+> char '#' <> initValue <> char '#'
    , text "#end"
    ]

-- pp arrays

ppTfmArrOut :: Bool -> Var -> Doc
ppTfmArrOut isInit v = ppVar v <> (if isInit then (text "[]") else empty)

ppArrIndex :: Var -> [Doc] -> Doc
ppArrIndex v as = ppVar v <> (hcat $ fmap brackets as)

ppArrVar :: Int -> Doc -> Doc
ppArrVar n v = v <> (hcat $ replicate n $ text "[]")

ppReadArr :: Var -> [Doc] -> Doc
ppReadArr v as = ppArrIndex v as

ppReadPureArr :: Doc -> [Doc] -> Doc
ppReadPureArr v as = v <> (hcat $ fmap brackets as)

ppWriteArr :: Var -> ArrIndex Doc -> Doc -> Doc
ppWriteArr v as b = ppArrIndex v as <+> equalsWord <+> b
    where equalsWord = if (varRate v == Sr) then text "strcpy" else equals

ppWriteInitArr :: Var -> [Doc] -> Doc -> Doc
ppWriteInitArr v as b = ppArrIndex v as <+> initWord <+> b
    where initWord = text $ if (varRate v == Sr) then "strcpy" else "init"

-------------------------------------

tab :: Monad m => Doc -> StateT TabDepth m Doc
tab doc = fmap (shiftByTab doc) get

tabWidth :: TabDepth
tabWidth = 4

shiftByTab :: Doc -> TabDepth -> Doc
shiftByTab doc n
    | n == 0    = doc
    | otherwise = indent (tabWidth * n) doc

left :: State TabDepth ()
left = modify pred

succTab :: Monad m => Doc -> StateT TabDepth m Doc
succTab doc = do
    a <- tab doc
    modify succ
    return a

prefix :: Text -> [Doc] -> Doc
prefix name args = textStrict name <> tupled args

ppCond :: Inline CondOp Doc -> Doc
ppCond = ppInline ppCondOp

($=) :: Doc -> Doc -> Doc
($=) a b = a <+> equals <+> b

ppOuts :: [R.Var] -> Doc
ppOuts xs = hsep $ punctuate comma $ map ppRatedVar xs

ppPrimOrVar :: PrimOr R.Var -> Doc
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

ppIf1, ppWhile, ppUntil :: Doc -> Doc -> Doc -> Doc

ppIf1 = ppIfBy "if"
ppWhile = ppIfBy "while"
ppUntil = ppIfBy "until"

ppIfBy :: Text -> Doc -> Doc -> Doc -> Doc
ppIfBy leadTag res p t = vcat
    [ textStrict leadTag <+> p <+> text "then"
    , text "    " <> res <+> char '=' <+> t
    , text "endif"
    ]

ppWhileRef :: Doc -> Var -> Doc -> Doc
ppWhileRef res p t = vcat
    [ textStrict "while" <+> ppVar p <+> text "then"
    , text "    " <> res <+> char '=' <+> t
    , text "endif"
    ]

ppOpc :: Doc -> Text -> [Doc] -> Doc
ppOpc out name xs = out <+> ppProc name xs

ppProc :: Text -> [Doc] -> Doc
ppProc name xs = textStrict name <+> (hsep $ punctuate comma xs)

ppVar :: Var -> Doc
ppVar v = case v of
    Var ty rate name   -> ppVarType ty <> ppRate rate <> textStrict (Text.cons (varPrefix ty) name)
    VarVerbatim _ name -> textStrict name

varPrefix :: VarType -> Char
varPrefix x = case x of
    LocalVar  -> 'l'
    GlobalVar -> 'g'

ppVarType :: VarType -> Doc
ppVarType x = case x of
    LocalVar  -> empty
    GlobalVar -> char 'g'

ppConvertRate :: Doc -> Rate -> Maybe Rate -> Doc -> Doc
ppConvertRate out to from var = case (to, from) of
    (Ar, Just Kr) -> upsamp var
    (Ar, Just Ir) -> upsamp $ toK var
    (Kr, Just Ar) -> downsamp var
    (Kr, Just Ir) -> out $= var
    (Ir, Just Ar) -> downsamp var
    (Ir, Just Kr) -> out $= toI var
    (Ar, Nothing) -> out $= toA var
    (Kr, Nothing) -> out $= toK var
    (Ir, Nothing) -> out $= toI var
    (a, Just b) | a == b -> out $= var
    (a, b)   -> error $ "bug: no rate conversion from " ++ show b ++ " to " ++ show a ++ "."
    where
        upsamp x = ppOpc out "upsamp" [x]
        downsamp x = ppOpc out "downsamp" [x]
        toA = func "a"
        toK = func "k"
        toI = func "i"

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

ppRatedVar :: R.Var -> Doc
ppRatedVar v = ppRate (R.varType v) <> int (R.varId v)

ppRate :: Rate -> Doc
ppRate x = case removeArrRate x of
    Sr -> char 'S'
    _  -> phi x
    where phi = textStrict . Text.toLower . Text.pack . show

ppTotalDur :: Double -> Doc
ppTotalDur d = text "f0" <+> double d

--------------------------------------------------------------
-- debug

newtype PrettyShowE = PrettyShowE E
newtype PrettyE = PrettyE E

instance Show PrettyShowE where
  show (PrettyShowE expr) = ppShow expr

instance Show PrettyE where
  show (PrettyE expr) = show $ ppE expr

ppE :: E -> Doc
ppE = foldFix go
  where
    go :: RatedExp Doc -> Doc
    go x = fromExp (fromInfo x) x

    fromInfo :: RatedExp Doc -> Doc
    fromInfo RatedExp{..} =
      hsep
        [ ppHash ratedExpHash
        , maybe mempty ppRate ratedExpRate
        , maybe mempty pretty ratedExpDepends
        ]

    ppHash = textStrict . Text.take 4 . Text.decodeUtf8 . Base64.encode . unExpHash

    fromExp :: Doc -> RatedExp Doc -> Doc
    fromExp info RatedExp{..} = indent 2 $ post $
      case ratedExpExp of
        ExpPrim p -> ppPrim p
        EmptyExp -> textStrict "EMPTY_EXPR"
        Tfm inf args -> ppTfm inf args
        ConvertRate to from a -> ppConvert to from a
        Select r n a -> ppSelect r n a
        If rate cond th el -> ppIff rate cond th el
        ExpBool args -> hsep ["some bool expr", pretty $ show args]
        ExpNum arg -> ppExpNum arg
        InitVar v a -> ppInitVar v a
        ReadVar v -> "ReadVar" <+> ppVar v
        WriteVar v a -> ppVar v $= pp a

        -- TODO
        InitArr _v _size -> undefined
        ReadArr _v _index -> undefined
        WriteArr _v _index _ -> undefined
        WriteInitArr _v _index _ -> undefined
        TfmArr _isInit _v _info _args -> undefined

        InitPureArr _outRate _procRate _vals -> undefined
        ReadPureArr _outRate _procRate _arr _index -> undefined

        IfBegin rate cond -> hsep ["IF", ppRate $ fromIfRate rate, ppCond $ fmap pp cond, "\n"]

        IfBlock rate cond (CodeBlock th) -> ppIfBlockBy "IF-BLOCK" rate cond th
        IfElseBlock rate cond (CodeBlock th) (CodeBlock el) ->
          ppFun (hsep ["IF-BLOCK", ppRate $ fromIfRate rate, ppCond $ fmap pp cond ])
            [ pp th
            , "ELSE-BLOCK"
            , pp el
            , "END-BLOCK"
            ]
        ElseBegin -> "ELSE"
        IfEnd -> "END_IF"
        UntilBegin rate cond -> hsep ["UNTIL", ppRate $ fromIfRate rate, ppCond $ fmap pp cond, "\n"]
        UntilEnd -> "END_UNTIL"
        WhileBegin rate cond -> hsep ["WHILE", ppRate $ fromIfRate rate, ppCond $ fmap pp cond, "\n"]
        WhileRefBegin v -> hsep ["WHILE_REF", ppVar v]
        WhileEnd -> "END_WHILE"

        UntilBlock rate cond (CodeBlock th) -> ppIfBlockBy "UNTIL-BLOCK" rate cond th
        WhileBlock rate cond (CodeBlock th) -> ppIfBlockBy "WHILE-BLOCK" rate cond th
        WhileRefBlock var (CodeBlock th) -> ppWhileRefBlock var th

        Verbatim txt -> ppFun "VERBATIM" [textStrict txt]
        Starts -> "STARTS"
        Seq a b -> vcat ["SEQ", pp a, pp b]
        Ends a -> vcat ["ENDS", pp a]
        InitMacrosInt _name _n  -> undefined
        InitMacrosDouble _name _d -> undefined
        InitMacrosString _name _str -> undefined
        ReadMacrosInt _name -> undefined
        ReadMacrosDouble _name -> undefined
        ReadMacrosString _name -> undefined
      where
        post a = hsep [hcat ["{",info, "}:"], a]

    ppIfBlockBy leadTag rate cond th =
      ppFun (hsep [leadTag, ppRate $ fromIfRate rate, ppCond $ fmap pp cond ])
        [ pp th
        , "END-BLOCK"
        ]

    ppWhileRefBlock var th =
      ppFun (hsep ["WHILE-REF-BLOCK", ppVar var])
        [ pp th
        , "END-BLOCK"
        ]

    ppTfm info args = ppFun (textStrict $ infoName info) (fmap pp args)

    ppConvert to from a =
      ppFun (hsep [textStrict "Convert-rate", ppRate to, maybe mempty ppRate from]) [pp a]

    ppSelect rate n arg =
      ppFun (hsep ["select", ppRate rate, pretty n]) [pp arg]

    ppIff rate cond th el =
      vcat
        [ hsep ["if", ppRate (fromIfRate rate), ppCond $ fmap pp cond]
        , indent 2 $ vcat
            [ "then" <+> pp th
            , "else" <+> pp el
            ]
        ]

    ppExpNum (PreInline op as) = ppNumOp op (fmap pp as)

    ppInitVar v a =
      ppFun (hsep ["InitVar", ppVar v]) [pp a]

    ppFun name args =
      vcat
        [ name
        , indent 2 $ vcat args
        ]

    pp = either ppPrim id . unPrimOr

