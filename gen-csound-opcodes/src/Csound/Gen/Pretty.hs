module Csound.Gen.Pretty(
    prettyModules, mainModule
) where

import qualified Data.Map as M

import Data.List
import Data.Char

import Text.PrettyPrint.Leijen

import Csound.Gen.Types

mainModule :: PackageType -> [Chap] -> String
mainModule packageType chaps = pp $ vCat
    [ text ("module Csound." ++ (show packageType) ++ ".Opcode") <+> nest 4 (parens $
        vcat $ punctuate comma $ fmap ((text "module" <+> ) . text . fullName packageType) names) <+> text "where"
    , vcat $ fmap ((text "import" <+> ) . text . fullName packageType) names
    ]
    where names = fmap nodeName chaps

-- noisy head
headBoom :: String -> [a] -> a
headBoom msg as = case as of
    []  -> error msg
    _   -> head as

vCat :: [Doc] -> Doc
vCat = vcat . punctuate line

------------------------------------------------------------------

type RenderOpc = Opc -> Doc

renderOpc :: PackageType -> RenderOpc
renderOpc x = case x of
    Dynamic -> prettyOpcDynamic
    Typed   -> pretty

data Import = SimpleImport String | QualifiedImport String String

imports :: PackageType -> [Import]
imports x = case x of
    Dynamic  -> [SimpleImport "Csound.Dynamic"]
    Typed    ->
        [ SimpleImport "Control.Applicative"
        , SimpleImport "Control.Monad.Trans.Class"
        , SimpleImport "Csound.Dynamic"
        , SimpleImport "Csound.Typed" ]

anAlias :: String
anAlias = "D"

------------------------------------------------------------------

prettyModules :: PackageType -> [Chap] -> [(String, String)]
prettyModules packageType =
    fmap $ \x -> (nodeName x, pp (pChap packageType (imports packageType) (renderOpc packageType) $ removeEmptySecs x))

pp a = displayS (renderPretty 0.5 200 a) ""

removeEmptySecs :: Chap -> Chap
removeEmptySecs a = a { nodeItems = filter (not . null . nodeItems) $ nodeItems a }

pChap :: PackageType -> [Import] -> RenderOpc -> Chap -> Doc
pChap packageType imports renderOpc a = vCat [chapHeader, chapImports, chapBody]
    where
        chapHeader  = hsHeader (fullName packageType $ nodeName a) (fmap getFuns $ nodeItems a)
        chapBody    = vCat $ fmap (pSec renderOpc $ nodeName a) $ nodeItems a
        chapImports = hsImports imports

        getFuns x = (nodeName x, fmap hsOpcName $ nodeItems x)

fullName :: PackageType -> String -> String
fullName packageType a = "Csound." ++ (show packageType) ++ ".Opcode." ++ a

pSec :: RenderOpc -> String -> Sec -> Doc
pSec renderOpc chapName a = vCat
    [ text "--" <+> text (nodeName a)
    , vCat $ fmap renderOpc $ nubBy (\a b -> opcName a == opcName b) $ nodeItems a ]

---------------------------------------------------------------------------------------
--

instance Pretty Opc where
    pretty a = vcat
        [ pretty $ opcDoc a
        , hsep [text (hsOpcName a), text "::",  opcTypedSignature a]
        , prettyTyCons a
        , hsep [ text "    where", prettyDynCons a ]]

---------------------------------------------------------------------------------------
-- pretty typed constructor

prettyTyCons :: Opc -> Doc
prettyTyCons a = hsep [name, args, char '=', cons, maybeReturn, text "f", consArgs]
    where
        name = text $ hsOpcName a

        maybeReturn
            | isConst   = text "$ return $"
            | otherwise = text "$"

        cons = case outTypes $ types $ opcSignature a of
            SingleOut SigOrD    -> text "fromGE"
            SE (SingleOut SigOrD) -> dirtySingle "fromGE"

            SingleOut ty        -> text $ show ty
            SE (SingleOut ty)   -> dirtySingle $ show ty

            OutNone             -> proc
            SE OutNone          -> proc

            Tuple               -> tup
            SE Tuple            -> seTup
            TheTuple _          -> tup
            SE (TheTuple _)     -> seTup

            OutTuple            -> tup
            SE OutTuple         -> seTup

            err                 -> error $ "no pretty for: " ++ show err

        dirtySingle x = hsep [text "fmap (", text x, text ". return) $ SE $ (depT =<<) $ lift"]
        proc = text "SE $ (depT_ =<<) $ lift"
        tup = text "pureTuple"
        seTup = text "dirtyTuple"

        args = onMidiArg $ hsep as

        consArgs
            | isConst   = empty
            | otherwise = (text "<$>" <+>)
                $ hsep $ intersperse (text "<*>") $ zipWith mkUn as ins

        isConst = insArity == 0

        as   = fmap ((char 'b' <>) . int) [1 .. insArity]
        insArity = length ins
        ins  = skipMsg $ case inTypes $ types $ opcSignature a of
            InTypes xs -> xs

        isListArg x = case x of
            TypeList _  -> True
            _           -> False

        skipMsg xs = case xs of
            Msg : ys -> ys
            _        -> xs

        onMidiArg x
            | isMsg     = char '_' <+> x
            | otherwise = x

        isMsg = case inTypes $ types $ opcSignature a of
            InTypes (Msg:_) -> True
            _               -> False

        mkUn arg x = case x of
            TypeList y  -> hsep [text "mapM", (text $ getUn y), arg]
            _           -> text (getUn x) <+> arg
            where
                getUn x = case x of
                    TypeList y  -> getUn y
                    SigOrD      -> "toGE"
                    _           -> "un" ++ show x

---------------------------------------------------------------------------------------
-- pretty dynamic constructor


prettyDynCons :: Opc -> Doc
prettyDynCons a = case verbatimBody $ opcName a of
    Just res            -> res
    Nothing | isOpcode  -> hsep [char 'f', args, char '=', cons, name, signature, consArgList]
    Nothing             -> ppOpr
    where
        cons
            | isMulti a = text "mopcs"
            | otherwise = text "opcs"

        args = hsep as

        name = dquotes $ text $ opcName a

        signature = pretty $ rates $ opcSignature a

        consArgList
            | noArgLists        = list as
            | onlyOneArgList    = head as
            | isSegr            = parens $ hsep [head as, text "++", list $ tail as]
            | otherwise         = parens $ hsep [list (init as), text "++", last as]

        as   = fmap ((char 'a' <>) . int) [1 .. insArity]
        insArity = length ins
        ins  = skipMsg $ case inTypes $ types $ opcSignature a of
            InTypes xs -> xs

        isListArg x = case x of
            TypeList _  -> True
            _           -> False


        noArgLists = all (not . isListArg) ins

        onlyOneArgList = case ins of
            [TypeList _] -> True
            _            -> False

        isSegr = case ins of
            [TypeList D, D, D]  -> True
            _                   -> False

        skipMsg xs = case xs of
            Msg : ys -> ys
            _        -> xs

        isOpcode = case rates $ opcSignature a of
            Single _   -> True
            Multi  _ _ -> True
            _          -> False

        ppOpr = case rates $ opcSignature a of
            Opr1    -> text $ "f a1 = opr1 \""  ++ opcName a ++ "\" a1"
            Opr1k   -> text $ "f a1 = opr1k \"" ++ opcName a ++ "\" a1"
            InfOpr  -> text $ "f a1 a2 = infOpr \""  ++ opcName a ++ "\" a1 a2"

        verbatimBody :: String -> Maybe Doc
        verbatimBody x = M.lookup x verbatimTab

        verbatimTab :: M.Map String Doc
        verbatimTab = fmap text $ M.fromList $ concat
            [ by
                (const $ "f a1 = oprBy \"urd\" [(Ar,[Kr]), (Kr,[Kr]), (Ir,[Ir])] [a1]")
                ["urd"]
            , by seg
                    ["linseg", "linsegb", "expseg"
                    , "cosseg", "cossegb" ]
            , [expsega, expsegb]
            , by segr  ["linsegr", "expsegr", "cossegr"]
            , by seg2
                    ["transeg", "transegb"]
            , by seg2r ["transegr"]
            ]
            where
                by f xs = zip xs (fmap f xs)

                seg  x = "f a1 = setRate Kr $ opcs \"" ++ x ++ "\" " ++ segRates
                    ++ " (a1 ++ [1, last a1])"
                seg2 x = "f a1 = setRate Kr $ opcs \"" ++ x ++ "\" " ++ segRates
                    ++ " (a1 ++ [1, 0, last a1])"

                segr x = "f a1 a2 a3 = setRate Kr $ opcs \"" ++ x ++ "\" " ++ segRates
                    ++ " (a1 ++ [1, last a1, a2, a3])"
                seg2r x = "f a1 a2 a3 a4 = setRate Kr $ opcs \"" ++ x ++ "\" " ++ segRates
                    ++ " (a1 ++ [1, 0, last a1, a2, a3, a4])"

                segRates = "[(Kr, repeat Ir), (Ar, repeat Ir)]"
                segaRates = "[(Ar, repeat Ir)]"

                expsega = ("expsega", "f a1 = opcs \"expsega\" [(Ar, repeat Ir)] (a1 ++ [1, last a1])")
                expsegb = ("expsegb", "f a1 = opcs \"expsegb\" [(Kr, repeat Ir), (Ar, repeat Ir)] (a1 ++ [1, last a1])")

---------------------------------------------------------------------------------------
-- pretty dynamic opcode

prettyOpcDynamic :: Opc -> Doc
prettyOpcDynamic a = vcat
    [ pretty $ opcDoc a
    , hsFun (hsOpcName a) (opcDynamicSignature a) (opcDynamicBody a) ]


opcDynamicSignature :: Opc -> Doc
opcDynamicSignature a = context <+> ins <+> outs
    where
        ins
            | isConstant a  = empty
            | otherwise     = text "[E] ->"

        outs = text $ case opcType a of
            PureSingle      -> "E"
            DirtySingle     -> "DepT m E"
            PureMulti       -> "MultiOut [E]"
            DirtyMulti      -> "MultiOut (DepT m [E])"
            Procedure       -> "DepT m ()"

        context
            | isPure a              = empty
            | otherwise             = text "Monad m =>"

opcDynamicBody :: Opc -> Doc
opcDynamicBody a = case verbatimBody (opcName a) of
    Just res -> res
    Nothing  | isOpcode  -> hsep [cons, name, signature, mConst]
    Nothing              -> ppOpr
    where
        cons = text $ case opcType a of
            PureSingle      -> "opcs"
            DirtySingle     -> "depT . opcs"
            PureMulti       -> "mopcs"
            DirtyMulti      -> "mdepT . mopcs"
            Procedure       -> "depT_ . opcs"

        name = dquotes $ text $ opcName a

        signature = pretty $ rates $ opcSignature a

        mConst
            | isConstant a  = text "$ []"
            | otherwise     = empty

        isOpcode = case rates $ opcSignature a of
            Single _   -> True
            Multi  _ _ -> True
            _          -> False

        ppOpr = case rates $ opcSignature a of
            Opr1    -> text $ "\\xs -> opr1 \""  ++ opcName a ++ "\" (head xs)"
            Opr1k   -> text $ "\\xs -> opr1k \"" ++ opcName a ++ "\" (head xs)"
            InfOpr  -> text $ "\\xs -> infOpr \""  ++ opcName a ++ "\" (head xs) (head $ tail xs)"

        verbatimBody :: String -> Maybe Doc
        verbatimBody x = case x of
            "urd"   -> Just $ text $ "depT . oprBy \"urd\" [(Ar,[Kr]), (Kr,[Kr]), (Ir,[Ir])]"
            _       -> Nothing


---------------------------------------------------------------------------------------
-- pretty typed opcode

prettyOpcTyped :: Opc -> Doc
prettyOpcTyped a = vcat
    [ pretty $ opcDoc a
    , hsFun (hsOpcName a) (opcTypedSignature a) (opcTypedBody a) ]

opcTypedSignature :: Opc -> Doc
opcTypedSignature = pretty . types . opcSignature

opcTypedBody :: Opc -> Doc
opcTypedBody a = case verbatimBody $ opcName a of
    Just res -> res
    Nothing | isConstant a -> hsep [cons, text "$ const", name]
    _ -> addArgs $ hsep [cons, name]
    where
        name = text $ qualifiedHsOpcName a
        cons = onMulti $ text $ firstLower $ show $ opcType a
        onMulti x = case opcType a of
            PureMulti   -> text "fromPm $" <+> x
            DirtyMulti  -> text "fromDm $" <+> x
            _           -> x

        -- assumption: one of the addSigOrDArgs or addMultiArgs
        -- is always identity function.
        addArgs = addSigOrDArgs . addMultiArgs

        addSigOrDArgs = case outTypes $ types $ opcSignature a of
            SingleOut SigOrD        -> args "fromGE $"
            SE (SingleOut SigOrD)   -> args "fmap fromGE $"
            _                       -> id
            where
                args fun body = case inTypes $ types $ opcSignature a of
                    InTypes xs | length xs == 0 -> text fun <+> body
                    InTypes xs                  ->
                        let args = fmap ((char 'a' <> ) . int) [1 .. length xs]
                        in  hsep [char '\\' <> hsep args, text "->", text fun, body, hsep $ zipWith convertSigOrDs xs args]
                convertSigOrDs ty arg = case ty of
                    SigOrD  -> parens $ text "toGE" <+> arg
                    _       -> arg

        addMultiArgs = case opcType a of
            PureMulti   -> args
            DirtyMulti  -> args
            _           -> id
            where
                args = case inTypes $ types $ opcSignature a of
                    InTypes xs | length xs == 0 -> id
                    InTypes xs                  ->
                        let args = hsep $ fmap ((char 'a' <> ) . int) [1 .. length xs]
                        in  \x -> hsep [char '\\' <> args, text "->", x, args]

        verbatimBody = flip M.lookup verbatimTab

        verbatimTab = fmap text $ M.fromList $ concat
            [ by seg
                    ["linseg", "linsegb", "expseg"
                    , "expsega"
                    , "expsegb", "cosseg", "cossegb" ]
            , by segr  ["linsegr", "expsegr", "cossegr"]
            , by seg2
                    ["transeg", "transegb"]
            , by seg2r ["transegr"]
            ]
            where
                by f xs = zip xs (fmap f xs)

                seg x = "kr . pureSingle D." ++ x ++ " . (\\xs -> xs ++ [1, last xs])"
                seg2 x = "kr . pureSingle D." ++ x ++ " . (\\xs -> xs ++ [1, 0, last xs])"

                segr x = "\\xs dt rx -> kr $ pureSingle D." ++ x ++ " (xs ++ [1, last xs]) dt rx"
                seg2r x = "\\xs dt rx -> kr $ pureSingle D." ++ x ++ " (xs ++ [1, 0, last xs]) dt rx"



---------------------------------------------------------------------------------------
-- pretty primitives

instance Pretty OpcDoc where
    pretty a =  vcat $ text "-- | " : intersperse (text "--") [desc, signature, link]
        where
            formDesc f = vcat $ fmap ((text "--" <+>) . text) $ lines $ renderDoc 70 $ text $ f a

            renderDoc n doc = displayS (renderPretty 0.2 n doc) ""

            desc
                | shortAndLongAreEqual  = shortDesc
                | otherwise             = vcat [shortDesc, text "--", longDesc]

            shortAndLongAreEqual = opcDocShortDescription a == opcDocLongDescription a

            shortDesc   = formDesc opcDocShortDescription
            longDesc    = formDesc opcDocLongDescription

            signature = vcat $ fmap ((text "-- >" <+>) . text) $  nestTail . lines =<< opcDocCode a
                where
                    nestTail xs = case xs of
                        []      -> []
                        [y]     -> [y]
                        y:ys    -> y : fmap ("    " ++ ) ys

            link = text "-- csound doc:" <+> text (inUrl $ fullPath $ opcDocLink a)
                where
                    fullPath x = x
                    inUrl x = "<" ++ x ++ ">"

instance Pretty Rates where
    pretty x = case x of
        Single rs -> pretty rs
        Multi a b -> pretty (a, b)
        SingleOpr rs -> pretty rs
        _ -> empty

instance Pretty RateList where
    pretty x = case x of
        JustList as -> pretty as
        Repeat rate -> parens $ text "repeat" <+> pretty rate
        Append a b  -> pretty a <+> text "++" <+> pretty b

instance Pretty Types where
    pretty a = hsep [pConstr (outTypes a), ins, pretty (outTypes a)]
        where
            pConstr x = case x of
                OutTuple    -> text "Tuple a =>"
                Tuple       -> text "Tuple a =>"
                -- assumption: SigOrD can be only in Single or SE output
                SingleOut SigOrD -> text $ "SigOrD " ++ nameSigOrD ++ " =>"
                SE a        -> pConstr a
                _           -> empty

            ins = case inTypes a of
                InTypes []  -> empty
                _           -> hsep [pretty $ inTypes a, text "->"]

instance Pretty InTypes where
    pretty (InTypes as) = hsep $ intersperse (text "->") $ fmap pretty as

instance Pretty OutTypes where
    pretty x = case x of
        OutTuple    -> text "a"
        Tuple       -> text "a"
        TheTuple ts -> tupled $ fmap pretty ts
        SingleOut a -> pretty a
        SE (SE a)   -> pretty (SE a)
        OutNone     -> text "SE ()"
        SE OutNone  -> text "SE ()"
        SE a        -> text "SE" <+> pretty a

instance Pretty Rate where
    pretty = text . show

instance Pretty Type where
    pretty x = case x of
        TypeList a  -> pretty [a]
        SigOrD      -> text nameSigOrD
        _           -> text $ show x

nameSigOrD = "a"

-----------------------------------------------------------

qualifiedHsOpcName :: Opc -> String
qualifiedHsOpcName a = anAlias ++ "." ++ hsOpcName a

hsOpcName :: Opc -> String
hsOpcName = resolveCollisions . toHsName . opcName
    where
        toHsName ('S' : 'T' : 'K' : rest) = "stk" ++ firstUpper rest

        toHsName x
            | isFirstUpper x = firstLower x
            | null pre  = x
            | null post = lowerPre
            | otherwise = lowerPre ++ firstUpper post
            where
                (pre, post) = span isUpper x
                lowerPre = fmap toLower pre

        isFirstUpper x = case x of
            (a:b:_) -> isUpper a && isLower b
            _       -> False

        resolveCollisions x
            | x `elem` specNames    = x ++ "'"
            | otherwise             = x

        specNames = ["in", "product", "sum", "max", "min", "print"]



--------------------------------------------------------------------
-- haskell syntax structures

hsHeader :: String -> [(String, [String])] -> Doc
hsHeader name secs = text "module" <+> text name <+> nest 4 (parens
    (vCat (line : (punctuate comma $ fmap (uncurry fromSec) secs)))) <+> text "where"
    where
        fromSec title funs = vcat
            [ if null title then empty else text "-- *" <+> text title
            , hsep $ punctuate comma $ fmap text funs ]

hsImports :: [Import] -> Doc
hsImports as = vcat $ fmap pretty as

instance Pretty Import where
    pretty x = case x of
        SimpleImport    name        -> text "import" <+> text name
        QualifiedImport name alias  -> text "import qualified" <+> text name <+> text "as" <+> text alias

hsFun :: String -> Doc -> Doc -> Doc
hsFun name ty body = vcat [hsDef name ty, hsConst name body]

hsDef :: String -> Doc -> Doc
hsDef name ty = text name <+> text "::" <+> ty

hsConst :: String -> Doc -> Doc
hsConst name body = text name <+> char '=' <+> body
