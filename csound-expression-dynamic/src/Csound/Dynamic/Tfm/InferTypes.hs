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
--      * the output is a minum of types of the branches
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

import Safe
import Control.Monad (zipWithM, foldM)
import Data.Semigroup (Min(..))
import Data.Maybe (fromMaybe)
import Data.List qualified as List
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.ByteString (ByteString)
import Data.Default
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet

import Csound.Dynamic.Const qualified as Const
import Csound.Dynamic.Types.Exp hiding (Var, varType)
import Csound.Dynamic.Types.Exp qualified as Exp

data OpcodeInferenceStrategy
  = PreferControlRate  -- prefer Kr-outputs for opcodes
  | PreferAudioRate    -- prefer Ar-outputs for opcodes
  deriving (Eq, Ord, Show, Read)

data OpcodeInferencePreference = OpcodeInferencePreference
  { preferControlOpcodes :: HashSet Name  -- ^ set of opcode names to use Kr by default
  , preferAudioOpcodes   :: HashSet Name  -- ^ set of opcode names to use Ar by default
  }
  deriving (Eq, Ord, Show, Read)

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

data InferenceOptions = InferenceOptions
  { opcodeInferenceStrategy    :: !OpcodeInferenceStrategy
  , opcodeInferencePreference  :: !OpcodeInferencePreference
  }
  deriving (Eq, Ord, Show, Read)

instance Default InferenceOptions where
  def = InferenceOptions
    { opcodeInferenceStrategy   = PreferControlRate
    , opcodeInferencePreference =
        OpcodeInferencePreference
          { preferControlOpcodes = Const.controlOpcodes
          , preferAudioOpcodes   = Const.audioOpcodes
          }
    }

data Stmt a = Stmt
  { stmtLhs :: !a
  , stmtRhs :: !(RatedExp a)
  }

data Var = Var
  { varType :: !Rate
  , varId   :: !Int
  }

data InferenceResult = InferenceResult
  { typedProgram       :: ![Stmt Var]
  , programLastFreshId :: !Int
  , programHasIfs      :: !Bool
      -- ^ does program has if-statemenrs
      -- we need it for the next optimization stage
  }

-- | Type-inference state
data St = St
  { stTypeMap     :: !(IntMap Rate)
      -- ^ types inferrred so far
  , stLastFreshId :: !Int
      -- ^ last fresh id (we use it to insert new variables for conversions)
  , stResult      :: ![Stmt Var]
      -- ^ typed program accumulated in reversed order
  , stConversions :: !(IntMap (Map Rate Var))
      -- ^ map to memorise conversions so far to not to convert twice
  , stPrims       :: Map Prim Var
      -- ^ sometimes we need to allocate new primitive value to convert it
  , stHasIfs      :: !Bool
  }

-- | Infer types/rates for a csound program
inferTypes :: InferenceOptions -> [Stmt Int] -> InferenceResult
inferTypes opts exprs = toResult $ execState (mapM_ (inferIter opts) exprs) initSt
  where
    toResult St{..} =
      InferenceResult
        { typedProgram  = List.reverse stResult
        , programLastFreshId = stLastFreshId
        , programHasIfs = stHasIfs
        }

    initSt =
      St
        { stTypeMap     = IntMap.empty
        , stLastFreshId = succ lastId
        , stResult      = []
        , stConversions = IntMap.empty
        , stPrims       = Map.empty
        , stHasIfs      = False
        }

    lastId = maybe 0 stmtLhs $ headMay $ List.reverse exprs

inferIter :: InferenceOptions -> Stmt Int -> State St ()
inferIter opts (Stmt lhs rhs) =
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
    ReadVar v -> onReadVar v
    WriteVar v arg -> onWriteVar v arg

    -- | Selects a cell from the tuple, here argument is always a tuple (result of opcode that returns several outputs)
    -- | if-then-else
    If ifRate cond th el -> onIf ifRate cond th el
    -- | Imperative If-then-else
    IfBegin ifRate cond -> onIfBegin ifRate cond
    ElseBegin -> saveProcedure ElseBegin
    IfEnd -> saveProcedure IfEnd

    -- | Verbatim stmt
    Verbatim txt -> saveProcedure (Verbatim txt)

    -- | Arrays
    InitArr _v _arrSize -> undefined -- !Var !(ArrSize a)
    ReadArr _v _index -> undefined -- !(ArrIndex a)
    WriteArr _v _index _val -> undefined --  !Var !(ArrIndex a) !a
    WriteInitArr _v _arrSize _initVal -> undefined -- !Var !(ArrIndex a) !a
    TfmArr _isArrInit _v _info _args -> undefined -- !IsArrInit !Var !Info ![a]

    -- | read macros arguments
    InitMacrosInt name n -> saveProcedure (InitMacrosInt name n)
    InitMacrosDouble name dbl -> saveProcedure (InitMacrosDouble name dbl)
    InitMacrosString name txt -> saveProcedure (InitMacrosString name txt)
    ReadMacrosInt name -> save Ir (ReadMacrosInt name)
    ReadMacrosDouble name -> save Ir (ReadMacrosDouble name)
    ReadMacrosString name -> save Ir (ReadMacrosString name)

    -- | looping constructions
    UntilBegin ifRate cond -> onUntilBegin ifRate cond
    UntilEnd -> saveProcedure UntilEnd
    WhileBegin ifRate cond -> onWhileBegin ifRate cond
    WhileRefBegin v -> saveProcedure (WhileRefBegin v)
    WhileEnd -> saveProcedure WhileEnd

    EmptyExp  -> saveProcedure EmptyExp

    -- | Dependency tracking
    Starts -> saveProcedure Starts
    Seq a b -> saveProcedure (Seq (setXr a) (setXr b))
    Ends a -> saveProcedure (Ends (setXr a))

  where
    setXr = fmap (Var Xr)

    onPrim p = save (primRate p) (ExpPrim p)

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
        if Map.size rateTab == 1
          then pure $ head $ Map.toList rateTab
          else
            case preferOpc opts (infoName info) rateTab of
              Right opcRate -> pure opcRate
              Left opcRates -> findSignature args opcRates
      onFixedRateTfm info signature args

    findSignature :: [PrimOr Int] -> [OpcSignature] -> State St OpcSignature
    findSignature args allOpcRates = go (head allOpcRates) Nothing allOpcRates
      where
        go :: OpcSignature -> Maybe SignatureChoice -> [OpcSignature] -> State St OpcSignature
        go defaultRate mBestFit candidateRates =
          case candidateRates of
            [] -> pure $ maybe defaultRate signatureCandidate mBestFit
            candidate : rest -> do
              scores <- tryCandidate candidate
              if isFit scores
                then pure candidate
                else go defaultRate (Just $ getBestFit scores mBestFit) rest

        tryCandidate :: OpcSignature -> State St SignatureChoice
        tryCandidate candidate@(_outRate, inRates) = do
          conversions <- countDestructiveConversions inRates
          pure $ SignatureChoice
            { destructiveConversionsCount = conversions
            , signatureCandidate = candidate
            }

        countDestructiveConversions :: [Rate] -> State St Int
        countDestructiveConversions rates = foldM countConversion 0 $ zip rates args

        countConversion :: Int -> (Rate, PrimOr Int) -> State St Int
        countConversion total (targetRate, arg) = do
          argVar <- mapM (getVar targetRate) arg
          let opcodeArg =
                OpcodeArg
                  { opcodeTo = targetRate
                  , opcodeFrom = varType <$> argVar
                  }
          pure $ if nonDestructive opcodeArg || unifies opcodeArg
            then total
            else total + 1

        isFit (SignatureChoice score _candidate) = score == 0

        getBestFit (SignatureChoice scores candidate) = \case
          Just (SignatureChoice prevScores prevCandidate) | prevScores < scores -> (SignatureChoice prevScores prevCandidate)
          _ -> (SignatureChoice scores candidate)

    onConvertRate toRate mFromRate arg = do
      fromRate <- maybe (either primRate varType . unPrimOr <$> mapM (getVar Ir) arg) pure mFromRate
      save toRate (ConvertRate toRate (Just fromRate) (Var fromRate <$> arg))

    onSelect rate outId arg =
      save rate (Select rate outId (Var Xr <$> arg))

    onInitVar v arg = save (Exp.varRate v) =<< typedRhs
      where
        typedRhs = do
          argVar <- mapM (getVar Ir) arg
          pure (InitVar v argVar)

    onReadVar v = save (Exp.varRate v) (ReadVar v)

    onWriteVar v arg = saveProcedure =<< typedRhs
      where
        typedRhs = do
          argVar <- mapM (getVar (Exp.varRate v)) arg
          pure $ WriteVar v argVar

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

    ----------------------------------------------------------------
    -- generic funs

    save :: Rate -> Exp Var -> State St ()
    save rate typedRhs =
      modify' $ \s -> s
        { stTypeMap = IntMap.insert lhs rate $ stTypeMap s
        , stResult  = Stmt (Var rate lhs) (rhs { ratedExpExp = typedRhs }) : stResult s
        }

    -- procedure does not save output rate to type map, as it's never going to
    -- be referenced from any right hand side of the expression
    --
    -- Procedures always have Xr as output rate
    saveProcedure :: Exp Var -> State St ()
    saveProcedure typedRhs =
      modify' $ \s -> s { stResult  = Stmt (Var Xr lhs) (rhs { ratedExpExp = typedRhs }) : stResult s }

type OpcSignature = (Rate, [Rate])

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
    Ir -> is Ir || isPrim
    _  -> is to
  where
    is r = either primRate id from == r

    isPrim = either (const True) (const False) from

-- | Checks if opcode conversion is non-destructive
nonDestructive :: OpcodeArg -> Bool
nonDestructive (OpcodeArg to (PrimOr from)) =
  case to of
    Xr -> True
    Ar -> True
    Kr -> fromRate /= Ar
    Ir -> fromRate /= Ar || fromRate /= Kr
    _  -> fromRate == to
  where
    fromRate = either primRate id from

applyArg :: Rate -> PrimOr Int -> State St (PrimOr Var)
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

getVar :: Rate -> Int -> State St Var
getVar defaultRate vid = do
  m <- gets stTypeMap
  let ty = fromMaybe defaultRate $ IntMap.lookup vid m
  pure (Var ty vid)

convert :: Rate -> PrimOr Var -> State St Var
convert toRate (PrimOr fromVar) = do
  case fromVar of
    Left p  -> convertPrim p
    Right v -> convertVar v
  where
    convertPrim :: Prim -> State St Var
    convertPrim prim = do
      primMap <- gets stPrims
      v <- case Map.lookup prim primMap of
        Just v  -> pure v
        Nothing -> allocatePrim prim
      convertVar v

    convertVar :: Var -> State St Var
    convertVar inVar = do
      let rhs = newExp $ ConvertRate toRate (Just $ varType inVar) (PrimOr $ Right inVar)
      outVar <- defineVar toRate rhs
      saveConversion outVar inVar
      pure outVar

    allocatePrim :: Prim -> State St Var
    allocatePrim prim = do
      var <- defineVar (primRate prim) (newExp $ ExpPrim prim)
      modify' $ \s -> s { stPrims = Map.insert prim var $ stPrims s }
      pure var

-- | Checks if convertion is identity, then returns original
convertIf :: Rate -> PrimOr Var -> State St (PrimOr Var)
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
ignoreHash :: ByteString
ignoreHash = ""

-- | Allocate new var and assign RHS expression to it
defineVar :: Rate -> RatedExp Var -> State St Var
defineVar rate rhs = do
  v <- freshVar rate
  saveStmt v rhs
  pure v

-- | Allocate fresh variable with given rate
freshVar :: Rate -> State St Var
freshVar rate = Var rate <$> freshId

-- | Allocate new fresh id
freshId :: State St Int
freshId = do
  lastFreshId <- gets stLastFreshId
  modify' $ \s -> s { stLastFreshId = lastFreshId + 1 }
  pure lastFreshId

insertBoolConverters :: Rate -> CondInfo (PrimOr Var) -> State St (CondInfo (PrimOr Var))
insertBoolConverters ifRate = mapM (mapM go)
  where
    go :: Var -> State St Var
    go v
      | ifRate >= varType v = pure v
      | otherwise           = convert ifRate (PrimOr $ Right v)

saveStmt :: Var -> RatedExp Var -> State St ()
saveStmt outVar typedRhs =
  modify' $ \s -> s
    { stTypeMap = IntMap.insert (varId outVar) (varType outVar) $ stTypeMap s
    , stResult  = Stmt outVar typedRhs : stResult s
    }

saveConversion :: Var -> Var -> State St ()
saveConversion outVar inVar =
  modify' $ \s -> s { stConversions = update $ stConversions s }
  where
    update conversionMap = IntMap.alter go (varId inVar) conversionMap
    go = Just . \case
      Nothing -> Map.singleton (varType outVar) outVar
      Just m  -> Map.insert (varType outVar) outVar m

setHasIfs :: State St ()
setHasIfs = modify' $ \s -> s { stHasIfs = True }

----------------------------------------------------------------
-- rate calculations

primRate :: Prim -> Rate
primRate = \case
  PrimString _ -> Sr
  PrimVar r _  -> r
  _            -> Ir

primOrRate :: PrimOr Var -> Rate
primOrRate = either primRate varType . unPrimOr
