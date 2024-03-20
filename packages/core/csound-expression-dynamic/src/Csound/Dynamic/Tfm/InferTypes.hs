-- | Algorithm to infer csound rates. It's type inference for Csound.
--
-- It proceeds from epxression leaves to the top of the expression tree while assigning the rates.
-- The expression is DAG defined as list which is sorted by dependencies from bottom to top.
--
-- We traverse over the list and assign types to the terms.
-- Assumptions:
--
--  * type of primitive values is Ir or Sr
--  * type of numeric expression is minimal type of it's arguments
--
--  * type of opcode is determined by the choice of the most fit signature to the arguments
--     unless it's required by the user to be of specific type.
--     We try to find the signature that leads to lesser amount of destructive conversions overall.
--
--  * If-then-else type:
--      * for condition it is derived form ifRate in the argument of If-constructor
--      * the output is a minimum of types of the branches
--
--  * procedures' output is asssigned with Xr type
--
--  Note on type ordering they go in order of definition from amount of memory used:
--   Xr | Ar | Kr | Ir
--
--   So the Ar is the minimum
module Csound.Dynamic.Tfm.InferTypes
  ( inferTypes
  , InferenceOptions (..)
  , InferenceResult (..)
  , OpcodeInferenceStrategy (..)
  , Stmt(..)
  , Var(..)
  ) where

import Control.Applicative ((<|>))
import Safe
import Control.Monad (zipWithM, foldM)
import Data.Semigroup (Min(..))
import Data.List qualified as List
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Default
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Vector.Mutable (STVector)
import Data.Vector.Mutable qualified as Vector
import Control.Monad.ST
import Data.Maybe (fromMaybe)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Text qualified as Text

import Csound.Dynamic.Const qualified as Const
import Csound.Dynamic.Types.Exp hiding (Var, varType)
import Csound.Dynamic.Types.Exp qualified as Exp
-- import Debug.Trace (trace)

-- core types

data Stmt a = Stmt
  { stmtLhs :: !a
  , stmtRhs :: !(RatedExp a)
  }
  deriving (Show, Eq, Ord)

data Var = Var
  { varType :: !Rate
  , varId   :: !Int
  }
  deriving (Show, Eq, Ord)

data InferenceResult = InferenceResult
  { typedProgram       :: ![Stmt Var]
  , programLastFreshId :: !Int
  , programHasIfs      :: !Bool
      -- ^ does program has if-statemenrs
      -- we need it for the next optimization stage
  }

-- option types

data InferenceOptions = InferenceOptions
  { opcodeInferenceStrategy    :: !OpcodeInferenceStrategy
  , opcodeInferencePreference  :: !OpcodeInferencePreference
  }
  deriving (Eq, Ord, Show, Read)

data OpcodeInferenceStrategy
  = PreferControlRate  -- prefer Kr-outputs for opcodes
  | PreferAudioRate    -- prefer Ar-outputs for opcodes
  deriving (Eq, Ord, Show, Read)

data OpcodeInferencePreference = OpcodeInferencePreference
  { preferControlOpcodes :: HashSet Name  -- ^ set of opcode names to use Kr by default
  , preferAudioOpcodes   :: HashSet Name  -- ^ set of opcode names to use Ar by default
  }
  deriving (Eq, Ord, Show, Read)


-- | Infer types/rates for a csound program
inferTypes :: InferenceOptions -> [Stmt Int] -> InferenceResult
inferTypes opts exprs = runST $ do
  env <- initEnv
  toResult <$> execStateT (mapM_ (inferIter opts) exprs) env
  where
    initEnv :: ST s (InferEnv s)
    initEnv = do
      typeMap <- Vector.replicate size Xr
      pure InferEnv
        { envTypeMap = typeMap
        , envConversions = IntMap.empty
        , envLastFreshId = size
        , envResult = []
        , envHasIfs = False
        , envPrims = Map.empty
        }

    toResult InferEnv{..} =
      InferenceResult
        { typedProgram  = List.reverse envResult
        , programLastFreshId = envLastFreshId
        , programHasIfs = envHasIfs
        }

    size = succ $ maybe 0 stmtLhs $ headMay $ List.reverse exprs

type Infer s a = StateT (InferEnv s) (ST s) a

-- | Type-inference state
data InferEnv s = InferEnv
  { envTypeMap     :: !(STVector s Rate)
      -- ^ types inferrred so far
  , envConversions :: !(IntMap (Map Rate Var))
     -- ^ conversions
  , envLastFreshId :: !Int
      -- ^ last fresh id (we use it to insert new variables for conversions)
  , envResult      :: ![Stmt Var]
      -- ^ typed program accumulated in reversed order
  , envPrims       :: Map Prim Var
      -- ^ sometimes we need to allocate new primitive value to convert it
  , envHasIfs      :: !Bool
  }

-------------------------------------------------------------------------------------
-- options

type OpcSignature = (Rate, [Rate])

preferOpc :: InferenceOptions -> Name -> Map Rate [Rate] -> Either [OpcSignature] OpcSignature
preferOpc (InferenceOptions strategy opcPrefs) name signatureMap
  | Just sig <- getControl = Right sig
  | Just sig <- getAudio   = Right sig
  | otherwise              = Left $
      case strategy of
        PreferControlRate -> List.reverse $ Map.toList signatureMap
        PreferAudioRate   -> Map.toList signatureMap
  where
    getControl = getBy Kr (preferControlOpcodes opcPrefs)
    getAudio = getBy Ar (preferAudioOpcodes opcPrefs)

    getBy rate s
      | HashSet.member name s = (rate, ) <$> Map.lookup rate signatureMap
      | otherwise = Nothing

instance Default InferenceOptions where
  def = InferenceOptions
    { opcodeInferenceStrategy   = PreferControlRate
    , opcodeInferencePreference =
        OpcodeInferencePreference
          { preferControlOpcodes = Const.controlOpcodes
          , preferAudioOpcodes   = Const.audioOpcodes
          }
    }

-------------------------------------------------------------------------------------
-- inference

inferIter :: forall s . InferenceOptions -> Stmt Int -> Infer s ()
inferIter opts (Stmt lhs rhs) =
  -- trace (unlines ["INFER RHS", show $ ratedExpExp rhs, show $ ratedExpRate rhs, "\n"]) $
  case ratedExpExp rhs of
    -- primitives
    ExpPrim p -> onPrim p

    -- | Application of the opcode: we have opcode information (Info) and the arguments [a]
    Tfm info args -> onTfm info args

    ConvertRate toRate fromRate a -> onConvertRate toRate fromRate a
    Select rate outId arg -> onSelect rate outId arg

    -- | Numerical expressions (rendered in infix notation in the Csound)
    ExpNum args -> onExpNum args
    ExpBool _ -> error "Bool Exp should be substituted"

    -- | Reading/writing a named variable
    InitVar v arg -> onInitVar v arg
    ReadVar ifRate v -> onReadVar ifRate (ratedExpRate rhs) v
    ReadVarTmp ifRate tmp v -> onReadVarTmp ifRate (ratedExpRate rhs) tmp v
    WriteVar ifRate v arg -> onWriteVar ifRate v arg

    -- | Selects a cell from the tuple, here argument is always a tuple (result of opcode that returns several outputs)
    -- | if-then-else
    If ifRate cond th el -> onIf ifRate cond th el
    -- | Imperative If-then-else
    IfBlock ifRate cond th -> onIfBlock ifRate cond th
    IfElseBlock ifRate cond th el -> onIfElseBlock ifRate cond th el
    IfBegin ifRate cond -> onIfBegin ifRate cond
    ElseBegin -> saveProcedure ElseBegin
    IfEnd -> saveProcedure IfEnd

    -- | Verbatim stmt
    Verbatim txt -> saveProcedure (Verbatim txt)

    -- | Arrays
    InitArr v arrSize -> onInitArr v arrSize
    ReadArr ifRate v index -> onReadArr ifRate v index
    ReadArrTmp ifRate tmp v index -> onReadArrTmp ifRate tmp v index
    WriteArr ifRate v index val -> onWriteArr ifRate v index val
    WriteInitArr ifRate v arrSize initVal -> onWriteInitArr ifRate v arrSize initVal
    TfmArr isArrInit v info args -> onTfmArr isArrInit v info args

    -- | Pure arrays (read-only)
    InitPureArr outRate procRate initVals -> onInitPureArr outRate procRate initVals
    ReadPureArr outRate procRate inArr index -> onReadPureArr outRate procRate inArr index

    -- | read macros arguments
    InitMacrosInt name n -> saveProcedure (InitMacrosInt name n)
    InitMacrosDouble name dbl -> saveProcedure (InitMacrosDouble name dbl)
    InitMacrosString name txt -> saveProcedure (InitMacrosString name txt)
    ReadMacrosInt name -> save Ir (ReadMacrosInt name)
    ReadMacrosDouble name -> save Ir (ReadMacrosDouble name)
    ReadMacrosString name -> save Ir (ReadMacrosString name)

    -- | looping constructions
    UntilBlock ifRate cond th -> onUntilBlock ifRate cond th
    WhileBlock ifRate cond th -> onWhileBlock ifRate cond th

    UntilBegin ifRate cond -> onUntilBegin ifRate cond
    UntilEnd -> saveProcedure UntilEnd
    WhileBegin ifRate cond -> onWhileBegin ifRate cond
    WhileEnd -> saveProcedure WhileEnd

    EmptyExp  -> saveProcedure EmptyExp

    -- | Dependency tracking
    Starts -> saveProcedure Starts
    Seq a b -> saveProcedure (Seq (setXr a) (setXr b))
    Ends a -> saveProcedure (Ends (setXr a))

    TfmInit _ _ _ -> error "No inference for TfmInit"
  where
    onPrim p = save rate (ExpPrim p)
      where
        rate = fromMaybe (primRate p) $ ratedExpRate rhs

    onTfm info args =
      case infoSignature info of
        MultiRate outRates inRates -> onMultiRateTfm info outRates inRates args
        SingleRate rateTab         -> onSingleRateTfm info rateTab args

    onMultiRateTfm info _outRates inRates args = do
      typedExpr <- Tfm info <$> zipWithM applyArg inRates args
      save Xr typedExpr

    onSingleRateTfm info rateTab args
      | Just userRates <- getUserDefinedRate = onFixedRateTfm info userRates args
      | otherwise                            = onFreeTfm info rateTab args
      where
        getUserDefinedRate = do
          userRate <- ratedExpRate rhs
          (userRate, ) <$> Map.lookup userRate rateTab

    onFixedRateTfm info (outRate, inRates) args = do
      typedExpr <- Tfm info <$> zipWithM applyArg inRates args
      save outRate typedExpr

    onFreeTfm info rateTab args = do
      signature <-
        case Map.toList rateTab of
          [rateInfo] -> pure rateInfo
          _ ->
            case preferOpc opts (infoName info) rateTab of
              Right opcRate -> pure opcRate
              Left opcRates -> findSignature args opcRates
      onFixedRateTfm info signature args

    findSignature :: [PrimOr Int] -> [OpcSignature] -> Infer s OpcSignature
    findSignature args allOpcRates = go (fromMaybe (Kr, []) $ headMay allOpcRates) Nothing allOpcRates
      where
        go :: OpcSignature -> Maybe SignatureChoice -> [OpcSignature] -> Infer s OpcSignature
        go defaultRate mBestFit candidateRates =
          case candidateRates of
            [] -> pure $ maybe defaultRate signatureCandidate mBestFit
            candidate : rest -> do
              scores <- tryCandidate candidate
              if isFit scores
                then pure candidate
                else go defaultRate (Just $ getBestFit scores mBestFit) rest

        tryCandidate :: OpcSignature -> Infer s SignatureChoice
        tryCandidate candidate@(_outRate, inRates) = do
          conversions <- countDestructiveConversions inRates
          pure $ SignatureChoice
            { destructiveConversionsCount = conversions
            , signatureCandidate = candidate
            }

        countDestructiveConversions :: [Rate] -> Infer s Int
        countDestructiveConversions rates = foldM countConversion 0 $ zip rates args

        countConversion :: Int -> (Rate, PrimOr Int) -> Infer s Int
        countConversion total (targetRate, arg) = do
          argVar <- mapM (getVar targetRate) arg
          let opcodeArg =
                OpcodeArg
                  { opcodeTo = targetRate
                  , opcodeFrom = varType <$> argVar
                  }
          pure $ if not (destructiveConversion opcodeArg) || unifies opcodeArg
            then total
            else total + 1

        isFit (SignatureChoice score _candidate) = score == 0

        getBestFit (SignatureChoice scores candidate) = \case
          Just (SignatureChoice prevScores prevCandidate) | prevScores < scores -> (SignatureChoice prevScores prevCandidate)
          _ -> (SignatureChoice scores candidate)

    onConvertRate toRate mFromRate arg = do
      fromRate <- maybe (either primRate varType . unPrimOr <$> mapM (getVar Ir) arg) pure mFromRate
      save toRate (ConvertRate toRate (Just fromRate) (Var fromRate <$> arg))

    setXr = fmap (Var Xr)

    onSelect rate outId arg =
      save rate (Select rate outId (Var Xr <$> arg))

    onInitVar v arg = save (Exp.varRate v) =<< typedRhs
      where
        typedRhs = do
          argVar <- mapM (getVar Ir) arg
          pure (InitVar v argVar)

    onReadVar ifRate mRate v =
      save (fromMaybe varRate mRate) (withConvert v)
      where
        varRate = Exp.varRate v

        withConvert var =
          case mRate of
            Nothing -> ReadVar ifRate var
            Just target | target == varRate -> ReadVar ifRate var
            Just target -> ExpPrim (PrimVar target var)

    onReadVarTmp ifRate mRate tmp v =
      save
        (fromMaybe (Exp.varRate v) ((if ifRate == IfIr then Just Ir else Nothing) <|> mRate <|> tmpVarRate tmp))
        (ReadVarTmp ifRate tmp v)

    onWriteVar ifRate v arg = saveProcedure =<< typedRhs
      where
        typedRhs = do
          argVar <- mapM (getVar (Exp.varRate v)) arg
          pure $ WriteVar ifRate v argVar

    onExpNum args = do
      argVars <- mapM (mapM $ getVar Ir) args
      save (numRate argVars) (ExpNum argVars)
      where
        numRate :: NumExp (PrimOr Var) -> Rate
        numRate e = max Ar $ getMin $ foldMap (Min . primOrRate) e

    onIf ifRate cond th el = do
      setHasIfs
      thVar <- mapM (getVar condMaxRate) th
      elVar <- mapM (getVar condMaxRate) el
      let rate = min (primOrRate thVar ) (primOrRate elVar)
      condVar <- mapM (mapM $ getVar condMaxRate) cond
      condVarSafe <- insertBoolConverters condMaxRate condVar
      case ifRate of
        IfIr -> saveIr rate condVarSafe thVar elVar
        IfKr -> saveKr rate condVarSafe thVar elVar
      where
        condMaxRate = fromIfRate ifRate

        saveIr rate condVarSafe thVar elVar
          | rate < Ir = do
              thVar1 <- convertIf Ir thVar
              elVar1 <- convertIf Ir elVar
              save Ir (If ifRate condVarSafe thVar1 elVar1)
          | otherwise = save rate (If ifRate condVarSafe thVar elVar)

        saveKr rate condVarSafe thVar elVar
          | rate == Ir = do
              thVar1 <- convertIf Kr thVar
              elVar1 <- convertIf Kr elVar
              save Kr (If ifRate condVarSafe thVar1 elVar1)
          | otherwise  = save rate (If ifRate condVarSafe thVar elVar)

    onIfBlock = onIfBlockBy IfBlock

    onUntilBlock = onIfBlockBy UntilBlock

    onWhileBlock = onIfBlockBy WhileBlock

    onIfBlockBy cons ifRate cond th = do
      setHasIfs
      condVar <- mapM (mapM $ getVar condMaxRate) cond
      condVarSafe <- insertBoolConverters condMaxRate condVar
      saveProcedure (cons ifRate condVarSafe (fmap (Var Xr) <$> th))
      where
        condMaxRate = fromIfRate ifRate

    onIfElseBlock ifRate cond th el = do
      setHasIfs
      condVar <- mapM (mapM $ getVar condMaxRate) cond
      condVarSafe <- insertBoolConverters condMaxRate condVar
      saveProcedure (IfElseBlock ifRate condVarSafe (fmap (Var Xr) <$> th) (fmap (Var Xr) <$> el))
      where
        condMaxRate = fromIfRate ifRate

    onIfBegin ifRate cond = do
      setHasIfs
      ifBeginBy IfBegin ifRate cond

    onWhileBegin = ifBeginBy WhileBegin
    onUntilBegin = ifBeginBy UntilBegin

    ifBeginBy cons ifRate cond = do
      condVar <- mapM (mapM $ getVar condMaxRate) cond
      condVarSafe <- insertBoolConverters condMaxRate condVar
      saveProcedure (cons ifRate condVarSafe)
      where
        condMaxRate = fromIfRate ifRate

    -------------------------------------------------------------
    -- arrays

    onInitArr v arrSize = do
      typedArrSize <- mapM (mapM (getVar Ir)) arrSize
      saveProcedure (InitArr v typedArrSize)

    onReadArr ifRate v index = save (Exp.varRate v) . ReadArr ifRate v =<< typedIndex
      where
        indexRate = getArrIndexRate v
        typedIndex = mapM (mapM (getVar indexRate)) index

    onReadArrTmp ifRate tmp v index = save (Exp.varRate v) . ReadArrTmp ifRate tmp v =<< typedIndex
      where
        indexRate = getArrIndexRate v
        typedIndex = mapM (mapM (getVar indexRate)) index

    onWriteArr ifRate v index arg = do
      typedIndex <- mapM (mapM (getVar indexRate)) index
      argVar <- mapM (getVar (Exp.varRate v)) arg
      saveProcedure (WriteArr ifRate v typedIndex argVar)
      where
        indexRate = getArrIndexRate v

    onWriteInitArr ifRate v arrSize initVal = do
      typedArrSize <- mapM (mapM (getVar Ir)) arrSize
      typedInitVal <- mapM (getVar (Exp.varRate v)) initVal
      saveProcedure (WriteInitArr ifRate v typedArrSize typedInitVal)

    getArrIndexRate v=
      case Exp.varRate v of
        Ir -> Ir
        Sr -> Ir
        _  -> Kr

    onTfmArr isArrInit vout info args = do
      typedArgs <- getTypedArrArgs args
      saveProcedure (TfmArr isArrInit vout info typedArgs)
      where
        outRate = Exp.varRate vout

        inRates =
          case infoSignature info of
            SingleRate rateMap ->
              case Map.lookup outRate rateMap of
                Just res -> res
                Nothing -> toError "Rate conversion is not supported for arrays"
            MultiRate _ _ -> toError "Arrays with multiple argument s are not supported"
          where
            toError msg = error (unwords [msg, "Found on array opcode", Text.unpack $ infoName info])

        getTypedArrArgs ins = zipWithM applyArg inRates ins

    -------------------------------------------------------------
    -- pure (read-only) arrays

    onInitPureArr outRate processingRate initVals = do
      typedInits <- mapM (mapM (getVar initRate)) initVals
      save (toArrRate outRate) (InitPureArr outRate processingRate typedInits)
      where
        initRate = fromIfRate processingRate

    onReadPureArr outRate processingRate arr index = do
      typedIndex <- mapM (getVar initRate) index
      typedArr <- mapM (getVar outRate) arr
      save outRate (ReadPureArr outRate processingRate typedArr typedIndex)
      where
        initRate = fromIfRate processingRate

    -------------------------------------------------------------
    -- generic funs

    save :: Rate -> Exp Var -> Infer s ()
    save rate typedRhs =
      saveStmt $ Stmt
        { stmtLhs = Var rate lhs
        , stmtRhs = rhs { ratedExpExp = typedRhs, ratedExpDepends = Nothing }
        }

    -- procedure does not save output rate to type map, as it's never going to
    -- be referenced from any right hand side of the expression
    --
    -- Procedures always have Xr as output rate
    saveProcedure :: Exp Var -> Infer s ()
    saveProcedure typedRhs =
      appendResult $ Stmt
        { stmtLhs = Var Xr lhs
        , stmtRhs = rhs { ratedExpExp = typedRhs, ratedExpDepends = Nothing }
        }

-------------------------------------------------------------
-- generic funs

setType :: Var -> Infer s ()
setType (Var rate name) = do
  typeMap <- gets envTypeMap
  Vector.write typeMap name rate

appendResult :: Stmt Var -> Infer s ()
appendResult expr = modify' $ \s -> s { envResult = expr : envResult s }

data SignatureChoice = SignatureChoice
  { destructiveConversionsCount :: !Int
  , signatureCandidate          :: !OpcSignature
  }

data OpcodeArg = OpcodeArg
  { opcodeTo   :: !Rate
  , opcodeFrom :: !(PrimOr Rate)
  }

unifies :: OpcodeArg -> Bool
unifies (OpcodeArg to (PrimOr from)) =
  case to of
    Xr -> True
    Ar -> is Ar
    Kr -> is Kr || is Ir || isPrim
    Ir -> is Ir
    _  -> is to
  where
    is r = either primRate id from == r

    isPrim = either (const True) (const False) from

-- | Checks if opcode conversion is destructive
-- Note that we rely on Haskell type-checker and don't consider
-- cases of type-mismatch lke comparing number with string.
--
-- There are two cases of destructive updates:
--
-- * Ar or Kr is converted to Ir
-- * Ar is converted to Kr
destructiveConversion :: OpcodeArg -> Bool
destructiveConversion (OpcodeArg to (PrimOr from)) =
  case to of
    Ir -> fromRate /= Ir
    Kr -> fromRate == Ar
    _  -> False
  where
    fromRate = either primRate id from

applyArg :: Rate -> PrimOr Int -> Infer s (PrimOr Var)
applyArg targetRate arg = do
  argVar <- mapM (getVar Ir) arg
  let opcArg =
        OpcodeArg
          { opcodeTo = targetRate
          , opcodeFrom = varType <$> argVar
          }
  if unifies opcArg
    then pure argVar
    else PrimOr . Right <$> convert (opcodeTo opcArg) argVar

-------------------------------------------------------------------
-- utils

getVar :: Rate -> Int -> Infer s Var
getVar _defaultRate vid = do
  types <- gets envTypeMap
  ty <- Vector.read types vid
  pure (Var ty vid)

convert :: Rate -> PrimOr Var -> Infer s Var
convert toRate (PrimOr fromVar) = do
  case fromVar of
    Left p  -> convertPrim p
    Right v -> convertVar v
  where
    convertPrim :: Prim -> Infer s Var
    convertPrim prim = do
      primMap <- gets envPrims
      v <- case Map.lookup prim primMap of
        Just v  -> pure v
        Nothing -> allocatePrim prim
      convertVar v

    convertVar :: Var -> Infer s Var
    convertVar inVar = do
      mOutVar <- tryExistingConverters inVar
      case mOutVar of
        Just outVar -> pure outVar
        Nothing     -> do
          let rhs = newExp $ ConvertRate toRate (Just $ varType inVar) (PrimOr $ Right inVar)
          outVar <- defineVar toRate rhs
          saveConversion outVar inVar
          pure outVar

    tryExistingConverters :: Var -> Infer s (Maybe Var)
    tryExistingConverters (Var _ name) = do
      convMap <- gets envConversions
      pure $ Map.lookup toRate =<< IntMap.lookup name convMap

    allocatePrim :: Prim -> Infer s Var
    allocatePrim prim = do
      var <- defineVar (primRate prim) (newExp $ ExpPrim prim)
      modify' $ \s -> s { envPrims = Map.insert prim var $ envPrims s }
      pure var

-- | Checks if convertion is identity, then returns original
convertIf :: Rate -> PrimOr Var -> Infer s (PrimOr Var)
convertIf toRate var
  | toRate == primOrRate var = pure var
  | otherwise                = PrimOr . Right <$> convert toRate var

newExp :: Exp a -> RatedExp a
newExp rhs =
  RatedExp
    { ratedExpHash = ignoreHash
    , ratedExpRate = Nothing
    , ratedExpDepends = Nothing
    , ratedExpExp = rhs
    }

-- | On this stage we don't need expression hashes anymore
ignoreHash :: ExpHash
ignoreHash = ExpHash ""

-- | Allocate new var and assign RHS expression to it
defineVar :: Rate -> RatedExp Var -> Infer s Var
defineVar rate rhs = do
  v <- freshVar rate
  appendResult (Stmt v rhs)
  pure v

-- | Allocate fresh variable with given rate
freshVar :: Rate -> Infer s Var
freshVar rate = Var rate <$> freshId

-- | Allocate new fresh id
freshId :: Infer s Int
freshId = do
  lastFreshId <- gets envLastFreshId
  modify' $ \s -> s { envLastFreshId = lastFreshId + 1 }
  pure lastFreshId

insertBoolConverters :: Rate -> CondInfo (PrimOr Var) -> Infer s (CondInfo (PrimOr Var))
insertBoolConverters ifRate = mapM (mapM go)
  where
    go :: Var -> Infer s Var
    go v
      | ifRate >= varType v = pure v
      | otherwise           = convert ifRate (PrimOr $ Right v)

saveStmt :: Stmt Var -> Infer s ()
saveStmt expr = do
  setType (stmtLhs expr)
  appendResult expr

saveConversion :: Var -> Var -> Infer s ()
saveConversion outVar inVar =
  modify' $ \s -> s { envConversions = update $ envConversions s }
  where
    update conversionMap = IntMap.alter go (varId inVar) conversionMap

    go = Just . \case
      Nothing -> Map.singleton (varType outVar) outVar
      Just m  -> Map.insert (varType outVar) outVar m

setHasIfs :: Infer s ()
setHasIfs = modify' $ \s -> s { envHasIfs = True }

----------------------------------------------------------------
-- rate calculations

primRate :: Prim -> Rate
primRate = \case
  PrimString _      -> Sr
  PrimVar r _       -> r
  P r _             -> r
  PrimInstrId instr -> instrIdRate instr
  _                 -> Ir

primOrRate :: PrimOr Var -> Rate
primOrRate = either primRate varType . unPrimOr
