module Csound.Dynamic.Render.Pretty (
  Doc,
  vcatSep,
  ppCsdFile,
  ppGen,
  ppNotes,
  ppInstr,
  ppStmt,
  ppTotalDur,
  PrettyE (..),
  PrettyShowE (..),
  ppE,
) where

import Control.Monad.Trans.State.Strict
import Data.IntMap qualified as IM

import Csound.Dynamic.Tfm.InferTypes qualified as R (Var (..))
import Csound.Dynamic.Types
import Data.ByteString.Base64 qualified as Base64
import Data.Fix (foldFix)
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Text.PrettyPrint.Leijen.Text
import Text.Show.Pretty (ppShow)

vcatSep :: [Doc] -> Doc
vcatSep = vcat . punctuate line

binaries, unaries :: Text -> [Doc] -> Doc
binaries op as = binary op (as !! 0) (as !! 1)
unaries op as = unary op (as !! 0)

binary :: Text -> Doc -> Doc -> Doc
binary op a b = parens $ a <+> textStrict op <+> b

unary :: Text -> Doc -> Doc
unary op a = parens $ textStrict op <> a

func :: Text -> Doc -> Doc
func op a = textStrict op <> parens a

ppCsdFile :: Doc -> Doc -> Doc -> [Plugin] -> Doc
ppCsdFile flags orc sco plugins =
  tag "CsoundSynthesizer" $
    vcatSep
      [ tag "CsOptions" flags
      , tag "CsInstruments" orc
      , tag "CsScore" sco
      , ppPlugins plugins
      ]

ppPlugins :: [Plugin] -> Doc
ppPlugins plugins = vcatSep $ fmap (\(Plugin name body) -> tag name (textStrict body)) plugins

tag :: Text -> Doc -> Doc
tag name content =
  vcatSep
    [ char '<' <> textStrict name <> char '>'
    , content
    , text "</" <> textStrict name <> char '>'
    ]

ppNotes :: InstrId -> [CsdEvent] -> Doc
ppNotes instrId = vcat . fmap (ppNote instrId)

ppNote :: InstrId -> CsdEvent -> Doc
ppNote instrId evt =
  char 'i'
    <+> ppInstrId instrId
    <+> double (csdEventStart evt)
    <+> double (csdEventDur evt)
    <+> hsep (fmap ppPrim $ csdEventContent evt)

ppStr :: Text -> Doc
ppStr = textStrict . Text.pack . show

ppPrim :: Prim -> Doc
ppPrim x = case x of
  P _rate n -> char 'p' <> int n
  PrimInstrId a -> ppInstrId a
  PString a -> int a
  PrimInt n -> int n
  PrimDouble d -> double d
  PrimString s -> ppStr s
  PrimVar targetRate v -> ppConverter targetRate (varRate v) $ ppVar v
  PrimTmpVar v -> ppTmpVar v
  where
    ppConverter dst src t
      | dst == src = t
      | dst == Ar && src == Kr = a (t)
      | dst == Ar && src == Ir = a (k (t))
      | dst == Kr = k (t)
      | dst == Ir && src == Kr = i (t)
      | dst == Ir && src == Ar = i (k (t))
      | otherwise = t
      where
        tfm ch v = hcat [char ch, parens v]
        a = tfm 'a'
        k = tfm 'k'
        i = tfm 'i'

ppTmpVar :: TmpVar -> Doc
ppTmpVar (TmpVar mRate _mInfo n) =
  "tmp_var_"
    <> int n
    <> maybe mempty (\r -> "_" <> ppTmpRate r) mRate
  where
    ppTmpRate = \case
      SingleTmpRate r -> ppRate r
      MultiTmpRate rs -> "multi_rate_" <> hcat (punctuate "_" (fmap ppRate rs))

ppGen :: Int -> Gen -> Doc
ppGen tabId ft =
  char 'f'
    <> int tabId
    <+> int 0
    <+> (int $ genSize ft)
    <+> (ppGenId $ genId ft)
    <+> (maybe empty (textStrict . Text.pack . show) $ genFile ft)
    <+> (hsep $ map double $ genArgs ft)

ppGenId :: GenId -> Doc
ppGenId x = case x of
  IntGenId a -> int a
  StringGenId a -> ppStr a

ppInstr :: InstrId -> Doc -> Doc
ppInstr instrId body =
  vcat
    [ text "instr" <+> ppInstrHeadId instrId
    , body
    , text "endin"
    ]

ppInstrHeadId :: InstrId -> Doc
ppInstrHeadId x = case x of
  InstrId den nom -> int nom <> maybe empty ppAfterDot den
  InstrLabel name -> textStrict name
  where
    ppAfterDot a = textStrict $ Text.pack $ ('.' :) $ reverse $ show a

ppInstrId :: InstrId -> Doc
ppInstrId x = case x of
  InstrId den nom -> int nom <> maybe empty ppAfterDot den
  InstrLabel name -> ppStr name
  where
    ppAfterDot a = textStrict $ Text.pack $ ('.' :) $ reverse $ show a

type TabDepth = Int

ppStmt :: [R.Var] -> Exp R.Var -> State TabDepth Doc
ppStmt outs expr = maybe (ppExp (ppOuts outs) expr) id (maybeStringCopy outs expr)

maybeStringCopy :: [R.Var] -> Exp R.Var -> Maybe (State TabDepth Doc)
maybeStringCopy outs expr = case (outs, expr) of
  ([R.Var Sr _], ExpPrim (PrimVar _rate var)) -> Just $ tab $ ppStringCopy IfIr (ppOuts outs) (ppVar var)
  ([R.Var Sr _], ReadVar ifRate var) -> Just $ tab $ ppStringCopy ifRate (ppOuts outs) (ppVar var)
  ([R.Var Sr _], ReadVarTmp ifRate _tmp var) -> Just $ tab $ ppStringCopy ifRate (ppOuts outs) (ppVar var)
  ([], WriteVar ifRate outVar a) | varRate outVar == Sr -> Just $ tab $ ppStringCopy ifRate (ppVar outVar) (ppPrimOrVar a)
  ([R.Var Sr _], ReadArr ifRate var as) -> Just $ tab $ ppStringCopy ifRate (ppOuts outs) (ppReadArr var $ fmap ppPrimOrVar as)
  ([], WriteArr ifRate outVar bs a) | varRate outVar == Sr -> Just $ tab $ ppStringCopy ifRate (ppArrIndex outVar $ fmap ppPrimOrVar bs) (ppPrimOrVar a)
  _ -> Nothing

ppStringCopy :: IfRate -> Doc -> Doc -> Doc
ppStringCopy ifRate outs src = ppOpc outs (strcpy ifRate) [src]

strcpy :: (IsString a) => IfRate -> a
strcpy = \case
  IfKr -> "strcpyk"
  IfIr -> "strcpy"

ppExp :: Doc -> Exp R.Var -> State TabDepth Doc
ppExp res expr = case fmap ppPrimOrVar expr of
  ExpPrim (PString n) -> tab $ ppStrget res n
  ExpPrim p -> tab $ res $= ppPrim p
  Tfm info [a, b] | isInfix info -> tab $ res $= binary (infoName info) a b
  Tfm info xs | isPrefix info -> tab $ res $= prefix (infoName info) xs
  Tfm info xs -> tab $ ppOpc res (infoName info) xs
  TfmInit _ _ _ -> error "TfmInit should not stay to the rendering stage, it's temporal"
  ConvertRate to from x -> tab $ ppConvertRate res to from x
  If _ifRate info t e -> tab $ ppIf res (ppCond info) t e
  ExpNum (PreInline op as) -> tab $ res $= ppNumOp op as
  WriteVar ifRate v a ->
    case ifRate of
      IfKr -> tab $ ppVar v $= a
      IfIr -> tab $ ppVar v <+> "init" <+> a
  InitVar v a -> tab $ ppOpc (ppVar v) "init" [a]
  ReadVar ifRate v ->
    case ifRate of
      IfIr -> tab $ res <+> "init" <+> ppVar v
      IfKr -> tab $ res $= ppVar v
  ReadVarTmp ifRate _tmp v ->
    case ifRate of
      IfIr -> tab $ res <+> "init" <+> ppVar v
      IfKr -> tab $ res $= ppVar v
  InitArr v as -> tab $ ppOpc (ppArrVar (length as) (ppVar v)) "init" as
  ReadArr ifRate v as ->
    tab $
      if (varRate v /= Sr)
        then case ifRate of
          IfKr -> res $= ppReadArr v as
          IfIr -> res <+> "init" <+> ppReadArr v as
        else res <+> strcpy ifRate <+> ppReadArr v as
  ReadArrTmp ifRate _tmp v as ->
    tab $
      if (varRate v /= Sr)
        then case ifRate of
          IfKr -> res $= ppReadArr v as
          IfIr -> res <+> "init" <+> ppReadArr v as
        else res <+> strcpy ifRate <+> ppReadArr v as
  WriteArr ifRate v as b -> tab $ ppWriteArr ifRate v as b
  WriteInitArr ifRate v as b -> tab $ ppWriteInitArr ifRate v as b
  TfmArr isInit v op [a, b] | isInfix op -> tab $ ppTfmArrOut isInit v <+> binary (infoName op) a b
  TfmArr isInit v op args | isPrefix op -> tab $ ppTfmArrOut isInit v <+> prefix (infoName op) args
  TfmArr isInit v op xs -> tab $ ppOpc (ppTfmArrOut isInit v) (infoName op) xs
  InitPureArr _outRate _procRate initVals -> tab $ ppOpc (ppArrVar 1 res) "fillarray" initVals
  ReadPureArr outRate _procRate arr index -> tab $ if (outRate /= Sr) then res $= ppReadPureArr arr [index] else res <+> text "strcpy" <+> ppReadPureArr arr [index]
  IfBegin _ a -> succTab $ text "if " <> ppCond a <> text " then"
  IfBlock _ cond (CodeBlock th) -> tab $ ppIf1 res (ppCond cond) th
  IfElseBlock _ cond (CodeBlock th) (CodeBlock el) -> tab $ ppIf res (ppCond cond) th el
  --     ElseIfBegin a                   -> left >> (succTab $ text "elseif " <> ppCond a <> text " then")
  ElseBegin -> left >> (succTab $ text "else")
  IfEnd -> left >> (tab $ text "endif")
  UntilBlock _ cond (CodeBlock th) -> tab $ ppUntil res (ppCond cond) th
  WhileBlock _ cond (CodeBlock th) -> tab $ ppWhile res (ppCond cond) th
  UntilBegin _ a -> succTab $ text "until " <> ppCond a <> text " do"
  UntilEnd -> left >> (tab $ text "od")
  WhileBegin _ a -> succTab $ text "while " <> ppCond a <> text " do"
  WhileEnd -> left >> (tab $ text "od")
  InitMacrosString name initValue -> tab $ initMacros (textStrict name) (textStrict initValue)
  InitMacrosDouble name initValue -> tab $ initMacros (textStrict name) (double initValue)
  InitMacrosInt name initValue -> tab $ initMacros (textStrict name) (int initValue)
  ReadMacrosString name -> tab $ res <+> text "strcpy" <+> readMacro name
  ReadMacrosDouble name -> tab $ res $= readMacro name
  ReadMacrosInt name -> tab $ res $= readMacro name
  EmptyExp -> return empty
  Verbatim str -> return $ textStrict str
  Select _rate _n a -> tab $ res $= ("SELECTS" <+> a)
  Starts -> tab $ res $= "STARTS"
  Seq a b -> tab $ hsep ["SEQ", a, b]
  Ends _a -> tab $ "ENDS"
  ExpBool _ -> tab "ExpBool"

-- x -> error $ "unknown expression: " ++ show x

-- pp macros

readMacro :: Text -> Doc
readMacro name = char '$' <> textStrict name

initMacros :: Doc -> Doc -> Doc
initMacros name initValue =
  vcat
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

ppWriteArr :: IfRate -> Var -> ArrIndex Doc -> Doc -> Doc
ppWriteArr ifRate v as b = ppArrIndex v as <+> equalsWord <+> b
  where
    equalsWord = if (varRate v == Sr) then strcpy ifRate else equals

ppWriteInitArr :: IfRate -> Var -> [Doc] -> Doc -> Doc
ppWriteInitArr ifRate v as b = ppArrIndex v as <+> initWord <+> b
  where
    initWord = text $ if (varRate v == Sr) then (strcpy ifRate) else "init"

-------------------------------------

tab :: (Monad m) => Doc -> StateT TabDepth m Doc
tab doc = fmap (shiftByTab doc) get

tabWidth :: TabDepth
tabWidth = 4

shiftByTab :: Doc -> TabDepth -> Doc
shiftByTab doc n
  | n == 0 = doc
  | otherwise = indent (tabWidth * n) doc

left :: State TabDepth ()
left = modify pred

succTab :: (Monad m) => Doc -> StateT TabDepth m Doc
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
ppIf res p t e =
  vcat
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
ppIfBy leadTag res p t =
  vcat
    [ textStrict leadTag <+> p <+> text "then"
    , text "    " <> res <+> char '=' <+> t
    , text "endif"
    ]

ppOpc :: Doc -> Text -> [Doc] -> Doc
ppOpc out name xs = out <+> ppProc name xs

ppProc :: Text -> [Doc] -> Doc
ppProc name xs = textStrict name <+> (hsep $ punctuate comma xs)

ppVar :: Var -> Doc
ppVar v = case v of
  Var ty rate name -> ppVarType ty <> ppRate rate <> textStrict (Text.cons (varPrefix ty) name)
  VarVerbatim _ name -> textStrict name

varPrefix :: VarType -> Char
varPrefix x = case x of
  LocalVar -> 'l'
  GlobalVar -> 'g'

ppVarType :: VarType -> Doc
ppVarType x = case x of
  LocalVar -> empty
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
  (a, b) -> hsep ["bug: no rate conversion from ", pretty (show b), "to", pretty (show a)]
  where
    upsamp x = ppOpc out "upsamp" [x]
    downsamp x = ppOpc out "downsamp" [x]
    toA = func "a"
    toK = func "k"
    toI = func "i"

-- expressions

ppInline :: (a -> [Doc] -> Doc) -> Inline a Doc -> Doc
ppInline ppNode a = iter $ inlineExp a
  where
    iter x = case x of
      InlinePrim n -> inlineEnv a IM.! n
      InlineExp op args -> ppNode op $ fmap iter args

-- booleans

ppCondOp :: CondOp -> [Doc] -> Doc
ppCondOp op = case op of
  TrueOp -> const $ text "(1 == 1)"
  FalseOp -> const $ text "(0 == 1)"
  And -> bi "&&"
  Or -> bi "||"
  Equals -> bi "=="
  NotEquals -> bi "!="
  Less -> bi "<"
  Greater -> bi ">"
  LessEquals -> bi "<="
  GreaterEquals -> bi ">="
  where
    bi = binaries

-- numeric

ppNumOp :: NumOp -> [Doc] -> Doc
ppNumOp op = case op of
  Add -> bi "+"
  Sub -> bi "-"
  Mul -> bi "*"
  Div -> bi "/"
  Neg -> uno "-"
  Pow -> bi "^"
  Mod -> bi "%"
  where
    bi = binaries
    uno = unaries

ppRatedVar :: R.Var -> Doc
ppRatedVar v = ppRate (R.varType v) <> int (R.varId v)

ppRate :: Rate -> Doc
ppRate x = case removeArrRate x of
  Sr -> char 'S'
  _ -> phi x
  where
    phi = textStrict . Text.toLower . Text.pack . show

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

    ppIndex index = either ppPrim id (unPrimOr index)

    ppSize sizes = parens ("Size" <+> hcat (fmap ppIndex sizes))

    ppIfRate = ppRate . fromIfRate

    fromExp :: Doc -> RatedExp Doc -> Doc
    fromExp info RatedExp{..} = indent 2 $
      post $
        case ratedExpExp of
          ExpPrim p -> ppPrim p
          EmptyExp -> textStrict "EMPTY_EXPR"
          Tfm inf args -> ppTfm inf args
          TfmInit v inf args -> ppTfmInit v inf args
          ConvertRate to from a -> ppConvert to from a
          Select r n a -> ppSelect r n a
          If rate cond th el -> ppIff rate cond th el
          ExpBool args -> hsep ["some bool expr", pretty $ show args]
          ExpNum arg -> ppExpNum arg
          InitVar v a -> ppInitVar v a
          ReadVar ifRate v -> "ReadVar" <+> ppIfRate ifRate <+> ppVar v
          ReadVarTmp ifRate tmp v -> hcat [ppTmpVar tmp, "=", "ReadVarTmp" <+> ppIfRate ifRate <+> ppVar v]
          WriteVar ifRate v a ->
            case ifRate of
              IfKr -> ppVar v $= pp a
              IfIr -> ppVar v <+> "init" <+> pp a
          InitArr v size -> "InitArr" <+> ppVar v <+> ppSize size
          ReadArr ifRate v index -> "ReadArr" <+> ppIfRate ifRate <+> ppVar v <+> (hcat $ fmap ppIndex index)
          ReadArrTmp ifRate tmp v index -> hcat [ppTmpVar tmp, "=", "ReadArrTmp", ppIfRate ifRate, ppVar v, (hcat $ fmap ppIndex index)]
          WriteArr ifRate v index _ -> "WriteArr" <+> ppIfRate ifRate <+> ppVar v <+> (hcat $ fmap ppIndex index)
          WriteInitArr ifRate v index _ -> "WriteInitArr" <+> ppIfRate ifRate <+> ppVar v <+> (hcat $ fmap ppIndex index)
          TfmArr isInit v inf args -> "TfmArr" <+> bool isInit <+> ppVar v <+> ppTfm inf args
          InitPureArr outRate procRate vals -> "InitPureArr" <+> ppRate outRate <+> ppIfRate procRate <+> (hcat $ fmap ppIndex vals)
          ReadPureArr outRate procRate arr index -> "ReadPureArr" <+> ppRate outRate <+> ppIfRate procRate <+> ppIndex arr <+> ppIndex index
          IfBegin rate cond -> hsep ["IF", ppRate $ fromIfRate rate, ppCond $ fmap pp cond, "\n"]
          IfBlock rate cond (CodeBlock th) -> ppIfBlockBy "IF-BLOCK" rate cond th
          IfElseBlock rate cond (CodeBlock th) (CodeBlock el) ->
            ppFun
              (hsep ["IF-BLOCK", ppRate $ fromIfRate rate, ppCond $ fmap pp cond])
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
          WhileEnd -> "END_WHILE"
          UntilBlock rate cond (CodeBlock th) -> ppIfBlockBy "UNTIL-BLOCK" rate cond th
          WhileBlock rate cond (CodeBlock th) -> ppIfBlockBy "WHILE-BLOCK" rate cond th
          Verbatim txt -> ppFun "VERBATIM" [textStrict txt]
          Starts -> "STARTS"
          Seq a b -> vcat ["SEQ", pp a, pp b]
          Ends a -> vcat ["ENDS", pp a]
          InitMacrosInt _name _n -> undefined
          InitMacrosDouble _name _d -> undefined
          InitMacrosString _name _str -> undefined
          ReadMacrosInt _name -> undefined
          ReadMacrosDouble _name -> undefined
          ReadMacrosString _name -> undefined
      where
        post a = hsep [hcat ["{", info, "}:"], a]

    ppIfBlockBy leadTag rate cond th =
      ppFun
        (hsep [leadTag, ppRate $ fromIfRate rate, ppCond $ fmap pp cond])
        [ pp th
        , "END-BLOCK"
        ]

    ppTfm info args = ppFun (textStrict $ infoName info) (fmap pp args)

    ppTfmInit v info args = hcat [ppTmpVar v, "=", ppFun (textStrict $ infoName info) (fmap pp args)]

    ppConvert to from a =
      ppFun (hsep [textStrict "Convert-rate", ppRate to, maybe mempty ppRate from]) [pp a]

    ppSelect rate n arg =
      ppFun (hsep ["select", ppRate rate, pretty n]) [pp arg]

    ppIff rate cond th el =
      vcat
        [ hsep ["if", ppRate (fromIfRate rate), ppCond $ fmap pp cond]
        , indent 2 $
            vcat
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
