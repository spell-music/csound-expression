{-# Language ScopedTypeVariables #-}
module Csound.Gen.Parse(
    parse, parseBy, DocTab, Unparsed, downloadDesc
) where

import Control.Applicative
import Control.Monad.Trans.Writer

import Control.Arrow(first, second)
import Data.Char
import Data.Ord
import Data.Function
import Data.Maybe
import Data.Either
import Data.List
import Data.Function

import qualified Data.Map as M
import qualified Data.Set as Set

import Data.List.Split
import Text.XML.Light hiding (Node)
import Network.HTTP.Conduit
import Control.Exception.Lifted(catch)
import System.FilePath

import Csound.Gen.Types

type Unparsed = (String, String, String)
type DocTab = M.Map String (String, String)

parse :: String -> ([Chap], [Unparsed])
parse = parseBy M.empty

parseBy :: DocTab -> String -> ([Chap], [Unparsed])
parseBy docTab = first toChap . parseOpcLines docTab . getSecs . getContent . rmSpecSymbols

rmSpecSymbols :: String -> String
rmSpecSymbols = rmBy "&nbsp;"
    where rmBy sub = concat . splitOn sub

-------------------------------------------------------------------------
-- parsing fsm

getSecs :: [Element] -> [Node OpcLine]
getSecs = fmap (filterNode (not . isBlackOpcode)) .  reverse . findSec []
    where
        findSec res [] = res

        findSec res (x:xs) = case getTitle x of
            Nothing                             -> findSec res xs
            Just title  | isInBlackList title   -> findSec res xs
            Just title                          -> completeSec res [] title xs

        completeSec res linesSoFar title [] = formRes title linesSoFar res

        completeSec res linesSoFar title (x:xs) = case getTitle x of
            Just a  -> findSec (formRes title linesSoFar res) (x:xs)
            Nothing -> case getOpcLine x of
                Just a  -> completeSec res (a:linesSoFar) title xs
                Nothing -> completeSec res linesSoFar title xs

        formRes title linesSoFar chaps = (Node title $ reverse linesSoFar) : chaps

isInBlackList :: String -> Bool
isInBlackList = ( `elem` blackList) . splitTitle

isBlackOpcode :: OpcLine -> Bool
isBlackOpcode = (flip Set.member blackOpcodesList) . opcLineName

blackList = fmap splitTitle $
    [ "Array Opcodes."
    , "Orchestra Syntax:Header."
    , "Orchestra Syntax:Block Statements."
    , "Orchestra Syntax:Macros."
    , "Instrument Control:Conditional Values."
    , "Instrument Control:Program Flow Control."
    , "Instrument Control:Initialization and Reinitialization."
    , "Real-time MIDI:Slider Banks."
    , "Lua Opcodes."
    , "Table Control:Table Queries."
    , "Table Control:Dynamic Selection."
    , "Table Control:Read/Write Opreations."
    , "Mathematical Operations:Arithmetic and Logic Operations."
    , "Mathematical Operations:Mathematical Functions. "
    , "Mathematical Operations:Trigonometric Functions."
    , "Python Opcodes."
    , "Utilities." ]

-- do not render opcodes from this list
blackOpcodesList = Set.fromList
    [ "turnoff2"
    , "JackoFreewheel"
    , "JackoInfo"
    , "pvs2tab"
    , "return"
    , "tab2pvs" ]

-------------------------------------------------------------------------
-- groups

parseOpcLines :: DocTab -> [Node OpcLine] -> ([Node Opc], [Unparsed])
parseOpcLines docTab = runWriter . mapM (\x -> fmap (Node (nodeName x)) $ uncurry (toOpc docTab) (splitTitle $ nodeName x) (nodeItems x))

toChap :: [Node Opc] -> [Chap]
toChap = fmap toNode . groupBy eqChapName
    where
        eqChapName = (==) `on` chapName
        toNode xs = Node (chapName $ head xs) (fmap toSec xs)
        toSec (Node sec opcs) = Node (snd $ splitTitle sec) opcs
        chapName = fst . splitTitle . nodeName

toOpc :: DocTab -> String -> String -> [OpcLine] -> Writer [Unparsed] [Opc]
toOpc docTab chapName secName = fmap catMaybes . mapM (fromOpcLines docTab chapName secName) . groupBy ((==) `on` opcLineName)

fromOpcLines :: DocTab -> String -> String -> [OpcLine] -> Writer [Unparsed] (Maybe Opc)
fromOpcLines docTab chapName secName as = case (rateHint echo <|> getRates echo as, typeHint echo <|> getTypes) of
    (Just rs, Just ts)  -> return $ Just $ Opc name (Signature rs ts) doc
    _                   -> tell unparsed >> return Nothing
    where
        name = opcLineName $ head as
        doc = OpcDoc getShortDescription getLongDescription (fmap toDocCode as) (opcLineLink $ head as)
            where toDocCode x = concat $ intersperse " " $ [opcLineOuts x, opcLineName x, opcLineIns x]

        getTypes = liftA2 Types (getInTypes echo $ head as) (getOutTypes chapName secName echo $ head $ reverse $ sortBy (comparing $ length . splitRates . opcLineOuts) as)

        unparsed = fmap (\x -> (opcLineOuts x, opcLineName x, opcLineIns x)) as

        echo = opcLineName $ head as

        getShortDescription = fst $ getDescription
        getLongDescription  = snd $ getDescription

        getDescription = maybe ("", "") id $ M.lookup name docTab

-------------------------------------------------------------------------
-- opc

data OpcLine = OpcLine
    { opcLineName   :: String
    , opcLineIns    :: String
    , opcLineOuts   :: String
    , opcLineLink   :: String
    } deriving (Show)

getOpcLine :: Element -> Maybe OpcLine
getOpcLine a
    | elIs "pre" a && rightName = fmap fixFLhvsBoxSetValueBug $ parseParts $ elContent a
    | otherwise = Nothing
    where
        rightName = case filterElements (elIs "a") a of
            [x] -> all (\y -> isAlphaNum y || y == '_') $ trim $ strContent x
            _   -> False

        fixFLhvsBoxSetValueBug x
            | opcLineName x == "FLhvsBox" && opcLineOuts x == "" = x { opcLineName = "FLhvsBoxSetValue" }
            | otherwise = x

        fixInitTabsForOscills x
            | opcLineName x == "oscil" || opcLineName x == "oscil3" = x { opcLineIns = phi $ opcLineIns x }
            | otherwise = x
            where phi = (++ ", ifn [, iphs]" ) . takeWhile (/= '[')


parseParts :: [Content] -> Maybe OpcLine
parseParts xs = case xs of
    Text outs : Elem name : Text ins : []   -> Just $ opcLine name (cdData ins) (cdData outs)
    Text outs : Elem name            : []   -> Just $ opcLine name ""           (cdData outs)
    Elem name : Text ins : []               -> Just $ opcLine name (cdData ins) ""
    Elem name            : []               -> Just $ opcLine name ""           ""
    _                                       -> Nothing
    where
        opcLine a ins outs = OpcLine (getName a) ins outs (getLink a)
        getName a = trim $ strContent a
        getLink a = maybe (error "getLink") id $ findAttrBy ((== "href") . qName) a

-------------------------------------------------------------------------
-- rates

type Echo = (String, String, String)

getRates :: String -> [OpcLine] -> Maybe Rates
getRates echo as = fmap fromRateLists $ getRateLists echo $ fmap (\x -> (opcLineOuts x, opcLineIns x)) as
    where
          debug x
            | echo == "prepiano" = error $ show x
            | otherwise = x

fromRateLists :: [(RateList, RateList)] -> Rates
fromRateLists rs
    | isSingleOutput    = Single $ fmap (\(x, y) -> (getSingleOutput x, y)) rs
    | isProcedure       = Single $ (\x -> [(Xr, snd x)]) $ head rs
    | otherwise         = uncurry Multi $ head $ reverse $ sortBy (longerRateList `on` fst) rs
    where
        isSingleOutput = all (isSingleRateList . fst) rs
        isProcedure = all (isEmptyRateList . fst) rs
        getSingleOutput = maybe (error "getSingleOutput") id . getSingleRateList

getRateLists :: String -> [(String, String)] -> Maybe [(RateList, RateList)]
getRateLists anOpcName = debug . mapM phi
    where phi (a, b) = liftA2 (,) (parseRateList echo a) (parseRateList echo b)
            where echo = (a, anOpcName, b)

          debug x = x {-
            | anOpcName == "prepiano" = error $ show x
            | otherwise = x -}

parseRateList :: Echo -> String -> Maybe RateList
parseRateList echo x = case splitOn ".." x of
    [a] -> withoutDots a
    a:_ -> withDots a
    where
        withoutDots = fmap JustList . rawRates
        withDots = (phi =<< ) . rawRates
            where phi as = case as of
                    []  -> Nothing
                    [a] -> Just $ Repeat a
                    _   ->
                        let lx = last as
                            ix = reverse $ dropWhile (== lx) $ reverse as
                        in  case ix of
                            []  -> Just $ Repeat lx
                            _   -> Just $ Append (JustList ix) (Repeat lx)
        rawRates a = mapM (getRate echo) $ splitRates a


splitRates :: String -> [String]
splitRates = filter (not . null) . splitOneOf "[], \\\n\160" . squashStr

squashStr :: String -> String
squashStr = go False
    where
        go flag xs = case xs of
            [] -> []
            '\"': rest -> '\"' : go (not flag) rest
            ' ' : rest -> if flag then go flag rest else ' ' : go flag rest
            a   : rest -> a : go flag rest

getRate :: Echo -> String -> Maybe Rate
getRate echo x = case x of
    -- files, and not ir-numbers
    "ifilname" -> Just Sr
    "ifilename" -> Just Sr
    "ifilcod" -> Just Sr
    "ifile" -> Just Sr
    "ifilhandle" -> Just Sr
    "ifilehandle" -> Just Sr
    -- globals
    'g' : _ -> getRate echo $ tail x
    -- normal rates
    'x' : _ -> Just Xr
    'a' : _ -> Just Ar
    'k' : _ -> Just Kr
    'i' : _ -> Just Ir
    'p' : _ -> Just Ir
    'S' : _ -> Just Sr
    '\"' : _ -> Just Sr
    'f' : _ -> Just Fr
    'w' : _ -> Just Wr
    'Q' : _ -> Just Xr
    "tvar"  -> Nothing -- Just Tvar
  --  '.' : _ -> Just Dots
    "soundfont" -> Just Ir
    "=" -> Nothing
    '(' : _ -> Nothing
    "condition" -> Nothing
    "label" -> Nothing
    "tab" -> Just Ir
    "Tinstrument" -> Nothing
    "Tsource1" -> Nothing
    "nresults" -> Nothing
    "OPTIONS" -> Nothing
    "options" -> Nothing
    "outfilename" -> Nothing
    '-' : _ -> Nothing
    "csound" -> Nothing
    "het_file" -> Nothing
    "cstext_file" -> Nothing
    _ -> error $ "unexpected rate: " ++ show echo ++ " " ++ x

-------------------------------------------------------------------------
-- types

getInTypes :: String -> OpcLine -> Maybe InTypes
getInTypes echo a = fmap (appendMidiMsg echo) $ case splitOn ".." str of
    [b] -> withoutDots b
    b:_ -> withDots b
    where
        str = takeWhile (/= '[') $ opcLineIns a
        withoutDots = fmap InTypes . rawElems
        withDots = (phi =<< ) . rawElems
            where phi as = case as of
                    [] -> Nothing
                    _  -> Just $ InTypes $ init as ++ [TypeList $ last as]
        rawElems x = mapM (getType (opcLineOuts a, echo, opcLineIns a)  (opcLineName a) ) $ splitRates x

getOutTypes :: String -> String -> String -> OpcLine -> Maybe OutTypes
getOutTypes chapName secName funName x = fmap checkEffects $ case splitOn ".." $ opcLineOuts x of
    [b] -> withoutDots b
    b:_ -> withDots b
    where
        withoutDots a = fmap parseSingleOrMultiOuts $ rawElems a
        withDots a = fmap parseMultiOuts $ rawElems a
        rawElems a = mapM (getType (opcLineOuts x, funName, opcLineIns x) (opcLineName x) ) $ splitRates a

        checkEffects
            | checkDirty chapName secName funName = SE
            | otherwise = id

        parseMultiOuts xs
            | all ( == Sig) xs = OutTuple
            | otherwise        = Tuple

        parseSingleOrMultiOuts xs = case xs of
            []  -> OutNone
            [a] -> SingleOut a
            xs | all ('[' /= ) (opcLineOuts x) && length xs < 5 -> TheTuple xs
            _   -> parseMultiOuts xs


getType :: Echo -> String -> String -> Maybe Type
getType echo opcName arg = fmap (toType arg) $ getRate echo arg
    where
        toType name rate = case rate of
            Xr  -> Sig
            Ar  -> Sig
            Kr | isTab name -> Tab
            Kr  -> Sig
            Ir | isTab name -> Tab
            Ir | isSf  name opcName -> Sf
            Ir  -> D
            Sr | isSf  name opcName -> Sf
            Sr  -> Str
            Fr  -> Spec
            Wr  -> Wspec
            Tvar -> TvarType

        isTab name = (length (splitOn "fn" name) > 1 || isTabN name)
        isSf name opcName =  "sf" `isPrefixOf` opcName && (name == "ipreindex" || name == "ifilhandle")

        isTabN x = (pref == "itab" || pref == "ktab") && (all isDigit suff)
            where (pref, suff) = splitAt 4 x

-------------------------------------------------------------------------
-- title

getTitle :: Element -> Maybe String
getTitle a
    | elIs "p" a    = fmap strContent $ filterElement (elIs "strong") a
    | otherwise     = Nothing

splitTitle :: String -> (String, String)
splitTitle x = (filter isAlphaNum a, filter (/= '\160') $ drop 1 b)
    where (a, b) = span (/= ':') x

-------------------------------------------------------------------------
-- content

getContent :: String -> [Element]
getContent = tail . elChildren . ( !! 1) . elChildren . maybe (error "getContent1") id . filterElement (elIs "body") . maybe (error "getContent2") id . parseXMLDoc

--------------------------------------------------------------------
-- pure / dirty

checkDirty :: String -> String -> String -> Bool
checkDirty chapName secName opcName = isDirtyChap || isDirtySec || isDirtyOpc
    where
        isDirtyChap = (`elem` dirtyChapList) chapName
        isDirtySec  = (`elem` dirtySecList)  secName
        isDirtyOpc  = (`elem` dirtyOpcList)  opcName

dirtyChapList = ["FLTK"]

dirtySecList = [ "Random (Noise) Generators." ]

dirtyOpcList =
    [ "delayr", "delayw", "deltap", "deltapi", "deltap3", "deltapx", "deltapxw"
    , "ftgen", "ftgenonce", "ftgentmp"
    , "rnd", "birnd"
    , "rspline", "jspline"
    , "vco2init"
    , "dssiinit"
    , "vstinit"
    , "OSCinit"
    , "MixerGetLevel", "MixerReceive"
    , "imagecreate", "imageload"
    , "fiopen"
    , "chnget", "chani", "chnrecv"
    , "serialBegin"
    , "OSClisten"
    , "JackoAudioIn"
    , "times", "timek"]

elIs :: String -> Element -> Bool
elIs name = (== name) . qName . elName

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

----------------------------------------------------------------------
-- hints

-- rates

rateHint :: String -> Maybe Rates
rateHint = flip M.lookup rateTab

rateTab = M.fromList $ concat
    [ opr1 [ "ampdb", "ampdbfs", "cent", "cpsoct", "octave", "semitone"]
    , opr1k
        [ "dbamp", "rnd", "birnd", "dbfsamp", "cpsmidinn"
        , "cpspch", "octcps", "octmidinn", "octpch"
        , "pchmidinn", "pchoct"]
    , infOpr []
    , return $ ("urd", SingleOpr [(Ar, JustList [Kr]), (Kr, JustList [Kr]), (Ir, JustList [Ir])])
    , return $ ("taninv2", Single [(Ar, JustList [Ar, Ar]), (Kr, JustList [Kr, Kr]), (Ir, JustList [Ir, Ir])])
    , return $ ("divz", Single [(Ar, JustList [Xr, Xr]), (Kr, JustList [Kr, Kr]), (Ir, JustList [Ir, Ir])])
    ]
    where

        opr tag names = fmap (\x -> (x, tag)) names

        opr1 = opr Opr1
        opr1k = opr Opr1k
        infOpr = opr InfOpr

-- types

typeHint :: String -> Maybe Types
typeHint = flip M.lookup typeTab

typeTab = M.fromList $ concat
            [ by osc ["oscil", "oscili", "oscil3", "poscil", "poscil3"]
            , by seg  segNames
            , by segr segrNames
            , by seg2r seg2rNames
            , by loopseg1 ["loopsegp"]
            , by loopseg2 ["looptseg", "lpsholdp"]
            , by loopseg3 ["loopseg", "loopxseg", "lpshold"]
            , by (fin Sig) ["fin", "fink"]
            , by (fin D) ["fini"]
            , by fout ["fout", "foutk"]
            , by fouti ["fouti", "foutir"]
            , by (fprints Sig) ["fprintks"]
            , by (fprints D) ["fprints"]
            , by inch ["inch"]
            , by inrg ["inrg", "outch", "outrg"]
            , by outc ["outc", "xout"]
            , by print' ["print"]
            , by printf_i ["printf_i"]
            , by printf ["printf"]
            , by printks ["printks"]
            , by prints ["prints"]
            , "multitap" # opc1 [Sig, TypeList D] Sig
            , by vbapNmove ["vbap16move", "vbap8move", "vbap4move", "vbapmove"]
            , "vbapzmove" # opc0 [Sig, D, D, D, TypeList D]
            , "platerev" # opcs [D, D, Sig, D, D, D, D, TypeList Sig]
            , "denorm" # opc0 [TypeList Sig]
            , "filter2" # opc1 [Sig, D, D, TypeList D] Sig
            , "zfilter2" # opc1 [Sig, Sig, Sig, D, D, TypeList D] Sig
            , "chebyshevpoly" # opc1 [Sig, TypeList Sig] Sig
            , "polynomial" # opc1 [Sig, TypeList Sig] Sig
            , by maxMin ["max", "min", "maxabs", "minabs", "sum", "product", "mac", "maca"]
            , "event" # opc0 [Str, Sig, Sig, Sig, TypeList Sig]
            , "event_i" # opc0 [Str, D, D, D, TypeList D]
            , "pset" # opc0 [TypeList D]
            , "changed" # opc1 [TypeList Sig] Sig
            , "splitrig" # opc0 [Sig, Sig, D, Tab, TypeList Sig]
            , "timedseq" # opc1 [Sig, Tab, TypeList Sig] Sig
            , "trigseq" # opc0 [Sig, Sig, Sig, Sig, Tab, TypeList Sig]
            , "push" # opc0 [TypeList Sig]
            , "subinstr" # opcs [D, TypeList D]
            , "subinstrinit" # opc0 [D, TypeList D]
            , "vphaseseg" # opc0 [Sig, D, D, TypeList D]
            , "clear" # opc0 [TypeList Sig]
            , "ctrlinit" # opc0 [TypeList D]
            , "OSClisten" # opc1e [D, D, D, TypeList Sig] Sig
            , "OSCsend" # opc0 [Sig, D, D, D, D, TypeList Sig]
            , "dssiaudio" # opcs [D, TypeList Sig]
            -- SigOrD cases
            , by (opc1 [SigOrD, Tab] SigOrD) ["table", "tablei", "table3"]
            , by rnd0 ["urandom"]
            , by rnd1 ["bexprnd", "cauchy", "duserrnd", "exprand", "linrand", "pcauchy", "poisson", "trirand", "unirand", "urd", "rnd", "birnd"]
            , by rnd2 ["random", "rnd31", "weibull"]
            , by rnd3 ["betarand", "cauchyi", "cuserrnd", "exprandi", "gaussi"]

            -- funs with arity 2
            , by (opc1 [SigOrD, SigOrD] SigOrD) ["taninv2", "divz"]

            -- simple funs
            , by fun1
                [ "ampdb", "ampdbfs", "cent", "cpsoct"
                , "octave", "semitone", "dbamp"
                , "dbfsamp", "cpsmidinn"
                , "cpspch", "octcps", "octmidinn"
                , "octpch", "pchmidinn", "pchoct"]
            ]
    where
        str # x = by x [str]
        by a xs = fmap (\x -> (x, a)) xs

        opc1 xs a = Types (InTypes xs) (SingleOut a)
        opc1e xs a = Types (InTypes xs) (SE $ SingleOut a)
        opc0 xs = Types (InTypes xs) (SE OutNone)
        opcs xs = Types (InTypes xs) (OutTuple)

        osc = opc1 [Sig, Sig, Tab] Sig
        seg = opc1 [TypeList D] Sig
        segr = opc1 [TypeList D, D, D] Sig
        seg2r = opc1 [TypeList D, D, D, D] Sig
        loopseg1 = opc1 [Sig, TypeList Sig] Sig
        loopseg2 = opc1 [Sig, Sig, TypeList Sig] Sig
        loopseg3 = opc1 [Sig, Sig, D, TypeList Sig] Sig

        segNames = ["linseg", "linsegb", "expseg", "expsega", "expsegb", "cosseg", "cossegb", "transeg", "transegb"]
        segrNames = filter (/= "transeg") $ fmap (++ "r") segNames
        seg2rNames = ["transegr"]

        fin x = opc0 [Str, D, D, TypeList x]
        fout = opc0 [Str, D, TypeList Sig]
        fouti = opc0 [Str, D, D, TypeList D]
        fprints x = opc0 [Str, Str, TypeList x]

        inch = opcs [TypeList Sig]
        inrg = opc0 [Sig, TypeList Sig]
        outc = opc0 [TypeList Sig]

        print' = opc0 [TypeList D]
        printf_i = opc0 [Str, D, TypeList D]
        printf = opc0 [Str, Sig, TypeList Sig]
        printks = opc0 [Str, D, TypeList Sig]
        prints = opc0 [Str, TypeList Sig]

        multitap = opc1 [Sig, TypeList D] Sig
        vbapNmove = opcs [Sig, D, D, D, TypeList D]
        maxMin = opc1 [TypeList Sig] Sig

        rnd0 = opc1e [] SigOrD
        rnd1 = opc1e [SigOrD] SigOrD
        rnd2 = opc1e [SigOrD, SigOrD] SigOrD
        rnd3 = opc1e [SigOrD, SigOrD, SigOrD] SigOrD

        fun1 = opc1 [SigOrD] SigOrD

appendMidiMsg :: String -> InTypes -> InTypes
appendMidiMsg name (InTypes xs)
    | name `elem` midiFuns =  InTypes (Msg : xs)
    | otherwise = InTypes xs

midiFuns =
    [ "ampmidi", "cpsmidi"
    , "cpsmidib", "cpstmid", "octmidi", "octmidib"
    , "pchmidi", "pchmidib", "veloc", "notnum", "pchbend"]

----------------------------------------------------------
-- download description

downloadDesc :: Opc -> IO (String, String)
downloadDesc opc = fmap (maybe ("", "") id . fmap getDesc) . simpleHttp' . fullPath . opcDocLink . opcDoc $ opc
    where
        -- fullPath x = "http://csound.com/docs/manual/" ++ x
        fullPath x = x

        getDesc x =
            ( trim $ dropOnLongHyphen $ orEmpty (getShortDesc =<< doc)
            , trim $ orEmpty (getLongDesc =<< doc))
            where doc = parseXMLDoc x

        orEmpty = maybe "" id

        getShortDesc = fmap getText . (filterElement (elIs "p") =<<) . filterElement elemShortDesc

        getLongDesc = fmap getText . (filterElement (elIs "p") =<<) . filterElement elemLongDesc

        elemShortDesc = any (== Attr (QName "class" Nothing Nothing) "refnamediv") . elAttribs
        elemLongDesc  = any ((=="Description"). strContent) . onlyElems  . elContent

        linkName = takeBaseName $ opcDocLink $ opcDoc opc

        titleIs name = (== Just name) . lookupAttrBy ((== "title") . qName) . elAttribs

        getText :: Element -> String
        getText elem = concat $ fmap go $ elContent elem
            where
                go x = case x of
                    Elem a -> getText a
                    Text cd -> cdData cd
                    CRef str -> ""

        dropOnLongHyphen xs = case splitOn "\226\128\148" xs of
            [a] -> a
            _:as -> concat as

        simpleHttp' x = catch (fmap Just $ simpleHttp x) $ \(e::HttpException) -> case e of
            _ -> do
                print $ "no page for: " ++ x
                return Nothing

