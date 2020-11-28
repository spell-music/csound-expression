{-# Language TypeFamilies, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, Rank2Types, CPP #-}
module Csound.Typed.Types.Prim(
    Sig(..), unSig, D(..), unD, Tab(..), unTab, Str(..), Spec(..), Wspec(..), renderTab,
    BoolSig(..), unBoolSig, BoolD(..), unBoolD, Unit(..), unit, Val(..), hideGE, SigOrD,
    Sig2, Sig3, Sig4, Sig5, Sig6, Sig7, Sig8,
    Sig2_2, Sig2_3, Sig2_4, Sig2_5, Sig2_6, Sig2_7, Sig2_8,
    D2, D3, D4, D5, D6,

    -- ** Tables
    preTab, preStringTab, TabSize(..), TabArgs(..), updateTabSize,
    fromPreTab, getPreTabUnsafe, skipNorm, forceNorm,
    nsamp, ftlen, ftchnls, ftsr, ftcps,
    TabList, tabList, fromTabList, fromTabListD,

    -- ** constructors
    double, int, text,

    -- ** constants
    idur, getSampleRate, getControlRate, getBlockSize, getZeroDbfs,
    getBpm, setBpm,

    -- ** converters
    ar, kr, ir, sig,

    -- ** lifters
    on0, on1, on2, on3,

    -- ** numeric funs
    quot', rem', div', mod', ceil', floor', round', int', frac',

    -- ** logic funs
    when1, whens, untilDo, whileDo, boolSig,
    equalsTo, notEqualsTo, lessThan, greaterThan, lessThanEquals, greaterThanEquals,
    whenD1, whenDs, untilDoD, whileDoD, untilBeginD,

    module Data.NumInstances.Tuple
) where

import Prelude hiding ((<*))

import Control.Applicative hiding ((<*))
import Control.Monad
import Control.Monad.Trans.Class
import Data.Monoid
import qualified Data.IntMap    as IM
import qualified Data.Map       as M

import Data.NumInstances.Tuple

import Control.Monad.Trans.Reader

import Data.Default
import Data.Boolean
import Data.String

import Csound.Dynamic hiding (double, int, str, when1, whens, ifBegin, ifEnd, elseBegin, untilBegin, untilEnd, untilDo, whileBegin, whileEnd, whileDo)
import qualified Csound.Dynamic as D(double, int, str, ifBegin, ifEnd, elseBegin, untilBegin, untilEnd, whileBegin, whileEnd)
import Csound.Typed.GlobalState.GE
import Csound.Typed.GlobalState.SE
import Csound.Typed.GlobalState.Options
import Csound.Typed.GlobalState.Opcodes(tableK, tableI)

-- | Signals
data Sig
    = Sig  (GE E)
    | PrimSig Double

unSig :: Sig -> GE E
unSig = toGE

-- | Constant numbers
data D
    = D  (GE E)
    | PrimD Double

unD :: D -> GE E
unD = toGE

-- | Strings
newtype Str  = Str  { unStr :: GE E }

-- | Spectrum. It's @fsig@ in the Csound.
newtype Spec  = Spec  { unSpec  :: GE E }

-- | Another type for spectrum. It's @wsig@ in the Csound.
newtype Wspec = Wspec { unWspec :: GE E }

type D2 = (D, D)
type D3 = (D, D, D)
type D4 = (D, D, D, D)
type D5 = (D, D, D, D, D)
type D6 = (D, D, D, D, D, D)

type Sig2 = (Sig, Sig)
type Sig3 = (Sig, Sig, Sig)
type Sig4 = (Sig, Sig, Sig, Sig)
type Sig5 = (Sig, Sig, Sig, Sig, Sig)
type Sig6 = (Sig, Sig, Sig, Sig, Sig, Sig)
type Sig7 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig)
type Sig8 = (Sig, Sig, Sig, Sig, Sig, Sig, Sig, Sig)

type Sig2_2 = (Sig2, Sig2)
type Sig2_3 = (Sig2, Sig2, Sig2)
type Sig2_4 = (Sig2, Sig2, Sig2, Sig2)
type Sig2_5 = (Sig2, Sig2, Sig2, Sig2, Sig2)
type Sig2_6 = (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2)
type Sig2_7 = (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2)
type Sig2_8 = (Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2, Sig2)

-- Booleans

-- | A signal of booleans.
data BoolSig
    = BoolSig (GE E)
    | PrimBoolSig Bool

unBoolSig :: BoolSig -> GE E
unBoolSig = toGE

-- | A constant boolean value.
data BoolD
    = BoolD (GE E)
    | PrimBoolD Bool

unBoolD :: BoolD -> GE E
unBoolD = toGE

type instance BooleanOf Sig  = BoolSig

type instance BooleanOf D    = BoolD
type instance BooleanOf Str  = BoolD
type instance BooleanOf Tab  = BoolD
type instance BooleanOf Spec = BoolD

-- Procedures

-- | Csound's empty tuple.
newtype Unit = Unit { unUnit :: GE () }

-- | Constructs Csound's empty tuple.
unit :: Unit
unit = Unit $ return ()

#if MIN_VERSION_base(4,11,0)
instance Semigroup Unit where
    (<>) = mappendUnit

instance Monoid Unit where
    mempty  = def

#else

instance Monoid Unit where
    mempty  = def
    mappend = mappendUnit

#endif

mappendUnit :: Unit -> Unit -> Unit
mappendUnit a b = Unit $ (unUnit a) >> (unUnit b)

instance Default Unit where
    def = unit

-- tables

-- | Tables (or arrays)
data Tab
    = Tab (GE E)
    | TabPre PreTab

preTab :: TabSize -> Int -> TabArgs -> Tab
preTab size gen args = TabPre $ PreTab size (IntGenId gen) args

preStringTab :: TabSize -> String -> TabArgs -> Tab
preStringTab size gen args = TabPre $ PreTab size (StringGenId gen) args

data PreTab = PreTab
    { preTabSize    :: TabSize
    , preTabGen     :: GenId
    , preTabArgs    :: TabArgs }

-- Table size.
data TabSize
    -- Size is fixed by the user.
    = SizePlain Int
    -- Size is relative to the renderer settings.
    | SizeDegree
    { hasGuardPoint :: Bool
    , sizeDegree    :: Int      -- is the power of two
    }

instance Default TabSize where
    def = SizeDegree
        { hasGuardPoint = False
        , sizeDegree = 0 }

-- Table arguments can be
data TabArgs
    -- absolute
    = ArgsPlain (Reader Int [Double])
{-    -- or relative to the table size (used for tables that implement interpolation)
    | ArgsRelative [Double]
    -- GEN 16 uses unusual interpolation scheme, so we need a special case
    | ArgsGen16 [Double] -}
    | FileAccess String [Double]

renderPreTab :: PreTab -> GE E
renderPreTab a = (fmap D.int . saveGen) =<< fromPreTab a

getPreTabUnsafe :: String -> Tab -> PreTab
getPreTabUnsafe msg x = case x of
    TabPre a    -> a
    _           -> error msg

fromPreTab :: PreTab -> GE Gen
fromPreTab a = withOptions $ \opt -> go (defTabFi opt) a
    where
        go :: TabFi -> PreTab -> Gen
        go tabFi tab = Gen size (preTabGen tab) args file
            where size = defineTabSize (getTabSizeBase tabFi tab) (preTabSize tab)
                  (args, file) = defineTabArgs size (preTabArgs tab)

getTabSizeBase :: TabFi -> PreTab -> Int
getTabSizeBase tf tab = case preTabGen tab of
    IntGenId intId -> IM.findWithDefault (tabFiBase tf) intId (tabFiGens tf)
    StringGenId stringId -> M.findWithDefault (tabFiBase tf) stringId (tabNamedFiGens tf)

defineTabSize :: Int -> TabSize -> Int
defineTabSize base x = case x of
       SizePlain n -> n
       SizeDegree guardPoint degree ->
                byGuardPoint guardPoint $
                byDegree base degree
    where byGuardPoint guardPoint
            | guardPoint = (+ 1)
            | otherwise  = id

          byDegree zero n = 2 ^ max 0 (zero + n)

defineTabArgs :: Int -> TabArgs -> ([Double], Maybe String)
defineTabArgs size args = case args of
    ArgsPlain as -> (runReader as size, Nothing)
    FileAccess filename as -> (as, Just filename)

-- | Skips normalization (sets table size to negative value)
skipNorm :: Tab -> Tab
skipNorm x = case x of
    Tab _ -> error "you can skip normalization only for primitive tables (made with gen-routines)"
    TabPre a -> TabPre $ a{ preTabGen = skipNormGenId $ preTabGen a }

skipNormGenId = mapIntGenId (negate . abs)

-- | Force normalization (sets table size to positive value).
-- Might be useful to restore normalization for table 'Csound.Tab.doubles'.
forceNorm :: Tab -> Tab
forceNorm x = case x of
    Tab _ -> error "you can force normalization only for primitive tables (made with gen-routines)"
    TabPre a -> TabPre $ a{ preTabGen = normGenId $ preTabGen a }

normGenId = mapIntGenId abs

mapIntGenId :: (Int -> Int) -> GenId -> GenId
mapIntGenId f genId = case genId of
    IntGenId intId -> IntGenId (f intId)
    _              -> genId

----------------------------------------------------------------------------
-- change table size

updateTabSize :: (TabSize -> TabSize) -> Tab -> Tab
updateTabSize phi x = case x of
    Tab _ -> error "you can change size only for primitive tables (made with gen-routines)"
    TabPre a -> TabPre $ a{ preTabSize = phi $ preTabSize a }

----------------------------------------------------------------------------
-- Tab of tabs

-- | Container list of tables
data TabList = TabList { unTabList :: GE E }

tabList :: [Tab] -> TabList
tabList xs = TabList $ saveTabs =<< mapM fromPreTab (getPreTabs xs)
    where
        getPreTabs xs = case xs of
            []            -> []
            Tab    _ : as -> getPreTabs as
            TabPre a : as -> a : getPreTabs as

fromTabList :: TabList -> Sig -> Tab
fromTabList ts knd = Tab $ do
    ets  <- toGE ts
    eknd <- toGE knd
    return $ tableK eknd ets

fromTabListD :: TabList -> D -> Tab
fromTabListD ts ind = Tab $ do
    ets  <- toGE ts
    eind <- toGE ind
    return $ tableI eind ets

-------------------------------------------------------------------------------
-- constructors

-- | Constructs a number.
double :: Double -> D
double = PrimD

-- | Constructs an integer.
int :: Int -> D
int =  PrimD . fromIntegral

-- | Constructs a string.
text :: String -> Str
text = fromE . D.str

instance IsString Str where
    fromString = text

-------------------------------------------------------------------------------
-- constants

-- | Querries a total duration of the note. It's equivallent to Csound's @p3@ field.
idur :: D
idur = fromE $ pn 3

getSampleRate :: D
getSampleRate = fromE $ readOnlyVar (VarVerbatim Ir "sr")

getControlRate :: D
getControlRate = fromE $ readOnlyVar (VarVerbatim Ir "kr")

getBlockSize :: D
getBlockSize = fromE $ readOnlyVar (VarVerbatim Ir "ksmps")

getZeroDbfs :: D
getZeroDbfs = fromE $ readOnlyVar (VarVerbatim Ir "0dbfs")

-- | Gets the global BPM value.
getBpm :: Sig
getBpm = fromE $ readOnlyVar bpmVar

-- | Sets the global BPM value.
setBpm :: Sig -> SE ()
setBpm x = fromDep_ $ hideGEinDep $ fmap (writeVar bpmVar) (toGE x)

-------------------------------------------------------------------------------
-- converters

-- | Sets a rate of the signal to audio rate.
ar :: Sig -> Sig
ar x = case x of
    PrimSig a -> PrimSig a
    Sig exp   -> Sig $ fmap (setRate Ar) exp

-- | Sets a rate of the signal to control rate.
kr :: Sig -> Sig
kr x = case x of
    PrimSig a -> PrimSig a
    Sig exp   -> Sig $ fmap (setRate Kr) exp

-- | Converts a signal to the number (initial value of the signal).
ir :: Sig -> D
ir x = case x of
    PrimSig a -> PrimD a
    Sig a -> D $ fmap (setRate Ir) a

-- | Makes a constant signal from the number.
sig :: D -> Sig
sig x = case x of
    PrimD a -> PrimSig a
    D exp   -> Sig $ fmap (setRate Kr) exp

-------------------------------------------------------------------------------
-- single wrapper

-- | Contains all Csound values.
class Val a where
    fromGE  :: GE E -> a
    toGE    :: a -> GE E

    fromE   :: E -> a
    fromE = fromGE . return

hideGE :: Val a => GE a -> a
hideGE = fromGE . join . fmap toGE

instance Val Sig    where
    fromGE = Sig

    toGE x = case x of
        Sig a       -> a
        PrimSig d   -> return $ D.double d

instance Val D      where
    fromGE  = D
    toGE x  = case x of
        D a     -> a
        PrimD d -> return $ D.double d

instance Val Str    where { fromGE = Str    ; toGE = unStr  }
instance Val Spec   where { fromGE = Spec   ; toGE = unSpec }
instance Val Wspec  where { fromGE = Wspec  ; toGE = unWspec}

instance Val TabList where { fromGE = TabList; toGE = unTabList }

instance Val Tab where
    fromGE = Tab
    toGE = unTab

unTab :: Tab -> GE E
unTab x = case x of
        Tab a -> a
        TabPre a -> renderPreTab a

renderTab :: Tab -> GE Int
renderTab x = case x of
    TabPre a -> saveGen =<< fromPreTab a
    Tab _    -> error "table should be primitive"

instance Val BoolSig where
    fromGE = BoolSig
    toGE x = case x of
        BoolSig a -> a
        PrimBoolSig b -> return $ if b then true else false

instance Val BoolD   where
    fromGE = BoolD
    toGE x = case x of
        BoolD a -> a
        PrimBoolD b -> return $ if b then true else false


class (IsPrim a, RealFrac (PrimOf a), Val a) => SigOrD a where

instance SigOrD Sig where
instance SigOrD D   where

on0 :: Val a => E -> a
on0 = fromE

on1 :: (Val a, Val b) => (E -> E) -> (a -> b)
on1 f a = fromGE $ fmap f $ toGE a

on2 :: (Val a, Val b, Val c) => (E -> E -> E) -> (a -> b -> c)
on2 f a b = fromGE $ liftA2 f (toGE a) (toGE b)

on3 :: (Val a, Val b, Val c, Val d) => (E -> E -> E -> E) -> (a -> b -> c -> d)
on3 f a b c = fromGE $ liftA3 f (toGE a) (toGE b) (toGE c)

op1 :: (Val a, Val b, IsPrim a, IsPrim b) => (PrimOf a -> PrimOf b) -> (E -> E) -> (a -> b)
op1 primFun exprFun x = maybe (on1 exprFun x) (fromPrim . primFun) (getPrim x)

op2 :: (Val a, Val b, Val c, IsPrim a, IsPrim b, IsPrim c) => (PrimOf a -> PrimOf b -> PrimOf c) -> (E -> E -> E) -> (a -> b -> c)
op2 primFun exprFun xa xb = case (getPrim xa, getPrim xb) of
    (Just a, Just b) -> fromPrim $ primFun a b
    _                -> on2 exprFun xa xb

-------------------------------------------------------------------------------
-- defaults

instance Default Sig    where def = 0
instance Default D      where def = 0
instance Default Tab    where def = fromE 0
instance Default Str    where def = text ""
instance Default Spec   where def = fromE 0

instance Default TabList where def = fromE 0

-------------------------------------------------------------------------------
-- monoid

#if MIN_VERSION_base(4,11,0)
instance Semigroup Sig where
    (<>) = on2 mappend

instance Monoid Sig where
    mempty = on0 mempty

#else

instance Monoid Sig where
    mempty  = on0 mempty
    mappend = on2 mappend

#endif


#if MIN_VERSION_base(4,11,0)
instance Semigroup D where
    (<>) = on2 mappend

instance Monoid D where
    mempty = on0 mempty

#else

instance Monoid D where
    mempty  = on0 mempty
    mappend = on2 mappend

#endif

-------------------------------------------------------------------------------
-- numeric

sigOn1 :: (Double -> Double) -> (E -> E) -> (Sig -> Sig)
sigOn1 numFun exprFun x = case x of
    PrimSig a -> PrimSig $ numFun a
    _         -> on1 exprFun x

sigOn2 :: (Double -> Double -> Double) -> (E -> E -> E) -> (Sig -> Sig -> Sig)
sigOn2 numFun exprFun xa xb = case (xa, xb) of
    (PrimSig a, PrimSig b) -> PrimSig $ numFun a b
    _                      -> on2 exprFun xa xb


instance Num Sig where
    { (+) = sigOn2 (+) (+); (*) = sigOn2 (*) (*); negate = sigOn1 negate negate
    ; (-) = sigOn2 (\a b -> a - b) (\a b -> a - b)
    ; fromInteger = PrimSig . fromInteger; abs = sigOn1 abs abs; signum = sigOn1 signum signum }

dOn1 :: (Double -> Double) -> (E -> E) -> (D -> D)
dOn1 numFun exprFun x = case x of
    PrimD a -> PrimD $ numFun a
    _         -> on1 exprFun x

dOn2 :: (Double -> Double -> Double) -> (E -> E -> E) -> (D -> D -> D)
dOn2 numFun exprFun xa xb = case (xa, xb) of
    (PrimD a, PrimD b) -> PrimD $ numFun a b
    _                      -> on2 exprFun xa xb

instance Num D where
    { (+) = dOn2 (+) (+); (*) = dOn2 (*) (*); negate = dOn1 negate negate
    ; (-) = dOn2 (\a b -> a - b) (\a b -> a - b)
    ; fromInteger = PrimD . fromInteger; abs = dOn1 abs abs; signum = dOn1 signum signum }

instance Fractional Sig  where { (/) = sigOn2 (/) (/);  fromRational = PrimSig . fromRational }
instance Fractional D    where { (/) = dOn2 (/) (/);    fromRational = PrimD . fromRational }

instance Floating Sig where
    { pi = PrimSig pi;  exp = sigOn1 exp exp;  sqrt = sigOn1 sqrt sqrt; log = sigOn1 log log; logBase = sigOn2 logBase logBase; (**) = sigOn2 (**) (**)
    ; sin = sigOn1 sin sin;  tan = sigOn1 tan tan;  cos = sigOn1 cos cos; sinh = sigOn1 sinh sinh; tanh = sigOn1 tanh tanh; cosh = sigOn1 cosh cosh
    ; asin = sigOn1 asin asin; atan = sigOn1 atan atan;  acos = sigOn1 acos acos ; asinh = sigOn1 asinh asinh; acosh = sigOn1 acosh acosh; atanh = sigOn1 atanh atanh }

instance Floating D where
    { pi = PrimD pi;  exp = dOn1 exp exp;  sqrt = dOn1 sqrt sqrt; log = dOn1 log log;  logBase = dOn2 logBase logBase; (**) = dOn2 (**) (**)
    ; sin = dOn1 sin sin;  tan = dOn1 tan tan;  cos = dOn1 cos cos; sinh = dOn1 sinh sinh; tanh = dOn1 tanh tanh; cosh = dOn1 cosh cosh
    ; asin = dOn1 asin asin; atan = dOn1 atan atan;  acos = dOn1 acos acos ; asinh = dOn1 asinh asinh; acosh = dOn1 acosh acosh; atanh = dOn1 atanh atanh }

class IsPrim a where
    type PrimOf a :: *
    getPrim :: a -> Maybe (PrimOf a)
    fromPrim :: PrimOf a -> a

instance IsPrim Sig where
    type PrimOf Sig = Double

    getPrim x = case x of
        PrimSig a -> Just a
        _         -> Nothing

    fromPrim = PrimSig

instance IsPrim D where
    type PrimOf D = Double

    getPrim x = case x of
        PrimD a -> Just a
        _         -> Nothing

    fromPrim = PrimD

instance IsPrim BoolSig where
    type PrimOf BoolSig = Bool

    getPrim x = case x of
        PrimBoolSig a -> Just a
        _         -> Nothing

    fromPrim = PrimBoolSig

instance IsPrim BoolD where
    type PrimOf BoolD = Bool

    getPrim x = case x of
        PrimBoolD a -> Just a
        _         -> Nothing

    fromPrim = PrimBoolD


ceil', floor', int', round' :: SigOrD a => a -> a
quot', rem', div', mod' :: SigOrD a => a -> a -> a

frac' :: (SigOrD a) => a -> a
frac' a = op1 (\x -> proxySnd a (properFraction x)) fracE a
    where
        proxySnd :: SigOrD a => a -> (Int, PrimOf a) -> PrimOf a
        proxySnd _ x = snd x

ceil' = op1 (\x -> fromIntegral ((ceiling x) :: Int)) ceilE
floor' = op1 (\x -> fromIntegral ((floor x) :: Int)) floorE
int' = op1 (\x -> fromIntegral ((truncate x) :: Int)) intE
round' = op1 (\x -> fromIntegral ((round x) :: Int)) roundE
quot' = op2 (\a b -> fromIntegral $ quot ((truncate a) :: Int) ((truncate b):: Int)) quot
rem' = op2 (\a b -> fromIntegral $ rem ((truncate a) :: Int) ((truncate b):: Int)) rem
div' = op2 (\a b -> fromIntegral $ div ((truncate a) :: Int) ((truncate b):: Int)) div
mod' = op2 (\a b -> fromIntegral $ mod ((truncate a) :: Int) ((truncate b):: Int)) mod

-------------------------------------------------------------------------------
-- logic

boolSigOn1 :: (Bool -> Bool) -> (E -> E) -> BoolSig -> BoolSig
boolSigOn1 = op1

boolSigOn2 :: (Bool -> Bool -> Bool) -> (E -> E -> E) -> BoolSig -> BoolSig -> BoolSig
boolSigOn2 = op2

boolDOn1 :: (Bool -> Bool) -> (E -> E) -> BoolD -> BoolD
boolDOn1 = op1

boolDOn2 :: (Bool -> Bool -> Bool) -> (E -> E -> E) -> BoolD -> BoolD -> BoolD
boolDOn2 = op2

instance Boolean BoolSig  where { true = PrimBoolSig True;  false = PrimBoolSig False;  notB = boolSigOn1 not notB;  (&&*) = boolSigOn2 (&&) (&&*);  (||*) = boolSigOn2 (||) (||*) }
instance Boolean BoolD    where { true = PrimBoolD   True;  false = PrimBoolD   False;  notB = boolDOn1   not notB;  (&&*) = boolDOn2   (&&) (&&*);  (||*) = boolDOn2   (||) (||*) }

instance IfB Sig  where
    ifB x a b = case x of
        PrimBoolSig cond -> if cond then a else b
        _                -> on3 ifB x a b

instance IfB D    where
    ifB x a b = case x of
        PrimBoolD cond -> if cond then a else b
        _              -> on3 ifB x a b

instance IfB Tab  where
    ifB x a b = case x of
        PrimBoolD cond -> if cond then a else b
        _              -> on3 ifB x a b

instance IfB Str  where
    ifB x a b = case x of
        PrimBoolD cond -> if cond then a else b
        _              -> on3 ifB x a b

instance IfB Spec where
    ifB x a b = case x of
        PrimBoolD cond -> if cond then a else b
        _              -> on3 ifB x a b

instance EqB Sig  where { (==*) = op2 (==) (==*);    (/=*) = op2 (/=) (/=*) }
instance EqB D    where { (==*) = op2 (==) (==*);    (/=*) = op2 (/=) (/=*) }

instance OrdB Sig where { (<*)  = op2 (<) (<*) ;    (>*)  = op2 (>) (>*);     (<=*) = op2 (<=) (<=*);    (>=*) = op2 (>=) (>=*) }
instance OrdB D   where { (<*)  = op2 (<) (<*) ;    (>*)  = op2 (>) (>*);     (<=*) = op2 (<=) (<=*);    (>=*) = op2 (>=) (>=*) }

-- | Invokes the given procedure if the boolean signal is true.
when1 :: BoolSig -> SE () -> SE ()
when1 xp body = case xp of
    PrimBoolSig p -> if p then body else return ()
    _             -> do
        ifBegin xp
        body
        ifEnd

-- | The chain of @when1@s. Tests all the conditions in sequence
-- if everything is false it invokes the procedure given in the second argument.
whens :: [(BoolSig, SE ())] -> SE () -> SE ()
whens bodies el = case bodies of
    []   -> el
    a:as -> do
        ifBegin (fst a)
        snd a
        elseIfs as
        elseBegin
        el
        foldl1 (>>) $ replicate (length bodies) ifEnd
    where elseIfs = mapM_ (\(p, body) -> elseBegin >> ifBegin p >> body)

ifBegin :: BoolSig -> SE ()
ifBegin a = fromDep_ $ D.ifBegin Kr =<< lift (toGE a)

ifEnd :: SE ()
ifEnd = fromDep_ D.ifEnd

elseBegin :: SE ()
elseBegin = fromDep_ D.elseBegin

-- | Invokes the given procedure if the boolean signal is true.
whenD1 :: BoolD -> SE () -> SE ()
whenD1 xp body = case xp of
    PrimBoolD p -> if p then body else return ()
    _             -> do
        ifBeginD xp
        body
        ifEnd

-- | The chain of @when1@s. Tests all the conditions in sequence
-- if everything is false it invokes the procedure given in the second argument.
whenDs :: [(BoolD, SE ())] -> SE () -> SE ()
whenDs bodies el = case bodies of
    []   -> el
    a:as -> do
        ifBeginD (fst a)
        snd a
        elseIfs as
        elseBegin
        el
        foldl1 (>>) $ replicate (length bodies) ifEnd
    where elseIfs = mapM_ (\(p, body) -> elseBegin >> ifBeginD p >> body)

ifBeginD :: BoolD -> SE ()
ifBeginD a = fromDep_ $ D.ifBegin Ir =<< lift (toGE a)

-- elseIfBegin :: BoolSig -> SE ()
-- elseIfBegin a = fromDep_ $ D.elseIfBegin =<< lift (toGE a)

untilDo :: BoolSig -> SE () -> SE ()
untilDo p body = do
    untilBegin p
    body
    untilEnd

whileDo :: BoolSig -> SE () -> SE ()
whileDo p body = do
    whileBegin p
    body
    whileEnd

whileBegin :: BoolSig -> SE ()
whileBegin a = fromDep_ $ D.whileBegin =<< lift (toGE a)

whileEnd :: SE ()
whileEnd = fromDep_ D.whileEnd

untilBegin :: BoolSig -> SE ()
untilBegin a = fromDep_ $ D.untilBegin =<< lift (toGE a)

untilEnd :: SE ()
untilEnd = fromDep_ D.untilEnd

untilDoD :: BoolD -> SE () -> SE ()
untilDoD p body = do
    untilBeginD p
    body
    untilEnd

whileDoD :: BoolD -> SE () -> SE ()
whileDoD p body = do
    whileBeginD p
    body
    whileEnd

whileBeginD :: BoolD -> SE ()
whileBeginD a = fromDep_ $ D.whileBegin =<< lift (toGE a)

untilBeginD :: BoolD -> SE ()
untilBeginD a = fromDep_ $ D.untilBegin =<< lift (toGE a)

-- | Creates a constant boolean signal.
boolSig :: BoolD -> BoolSig
boolSig x = case x of
    PrimBoolD b -> PrimBoolSig b
    BoolD a     -> BoolSig a

infix  4  `equalsTo`, `notEqualsTo`, `lessThan`, `lessThanEquals`, `greaterThanEquals`, `greaterThan`

equalsTo :: EqB a => a -> a -> BooleanOf a
equalsTo = (==*)

notEqualsTo :: EqB a => a -> a -> BooleanOf a
notEqualsTo = (/=*)

lessThan :: OrdB a => a -> a -> BooleanOf a
lessThan = (<*)

greaterThan :: OrdB a => a -> a -> BooleanOf a
greaterThan = (>*)

lessThanEquals :: OrdB a => a -> a -> BooleanOf a
lessThanEquals = (<=*)

greaterThanEquals :: OrdB a => a -> a -> BooleanOf a
greaterThanEquals = (>=*)

----------------------------------------------

-- | nsamp — Returns the number of samples loaded into a stored function table number.
--
-- > nsamp(x) (init-rate args only)
--
-- csound doc: <http://www.csounds.com/manual/html/nsamp.html>
nsamp :: Tab -> D
nsamp = on1 $ opr1 "nsamp"

-- | Returns a length of the table.
ftlen :: Tab -> D
ftlen = on1 $ opr1 "ftlen"

-- | Returns the number of channels for a table that stores wav files
ftchnls :: Tab -> D
ftchnls = on1 $ opr1 "ftchnls"

-- | Returns the sample rate for a table that stores wav files
ftsr :: Tab -> D
ftsr = on1 $ opr1 "ftsr"

-- | Returns the base frequency for a table that stores wav files
ftcps :: Tab -> D
ftcps = on1 $ opr1 "ftcps"


-------------------------------------------------
-- numeric instances

instance (Num a1, Num a2, Num a3, Num a4, Num a5, Num a6, Num a7, Num a8) => Num (a1, a2, a3, a4, a5, a6, a7, a8) where
    (a1, a2, a3, a4, a5, a6, a7, a8) + (b1, b2, b3, b4, b5, b6, b7, b8) = (a1 + b1, a2 + b2, a3 + b3, a4 + b4, a5 + b5, a6 + b6, a7 + b7, a8 + b8)
    (a1, a2, a3, a4, a5, a6, a7, a8) * (b1, b2, b3, b4, b5, b6, b7, b8) = (a1 * b1, a2 * b2, a3 * b3, a4 * b4, a5 * b5, a6 * b6, a7 + b7, a8 + b8)
    negate (a1, a2, a3, a4, a5, a6, a7, a8) = (negate a1, negate a2, negate a3, negate a4, negate a5, negate a6, negate a7, negate a8)

    fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
    signum (a1, a2, a3, a4, a5, a6, a7, a8) = (signum a1, signum a2, signum a3, signum a4, signum a5, signum a6, signum a7, signum a8)
    abs (a1, a2, a3, a4, a5, a6, a7, a8) = (abs a1, abs a2, abs a3, abs a4, abs a5, abs a6, abs a7, abs a8)

instance (Fractional a1, Fractional a2, Fractional a3, Fractional a4, Fractional a5, Fractional a6, Fractional a7, Fractional a8) => Fractional (a1, a2, a3, a4, a5, a6, a7, a8) where
    recip (a1, a2, a3, a4, a5, a6, a7, a8) = (recip a1, recip a2, recip a3, recip a4, recip a5, recip a6, recip a7, recip a8)
    fromRational n = (fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n, fromRational n)
