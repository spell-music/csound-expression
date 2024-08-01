-- | Internal state for typed core of the EDSL
module Csound.Core.State (
  Run (..),
  Dep,
  localy,
  exec,
  insertInstr,
  insertNamedInstr,
  insertNote,
  setTotalDur,
  insertGlobalExpr,
  initGlobalVar,
  initClearableGlobalVar,
  initGlobalArrVar,
  isGlobalInstr,
  withCurrentRate,
  getCurrentRate,
  saveGen,
  saveTabs,
  getOptions,
  getReadOnlyVar,
  getReadOnlyVars,
  clearVarsAct,

  -- * Vco
  VcoInit (..),
  VcoShape (..),
  saveVco,

  -- * Options
  setOption,
  setDefaultOption,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Csound.Dynamic.Debug (IsDebug, traceShowIf)
import Csound.Dynamic.Render.Pretty (ppE)
import Data.Default (def)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (doesFileExist)

import Csound.Core.Render.Options (Options (csdUdos), UdoDef (..), Udos (..))
import Csound.Core.Render.Options qualified as Options
import Csound.Dynamic

-- | Monad for dependency tracking augmented with internal state
type Dep a = DepT Run a

-- | Monad for typed Csound expressions
newtype Run a = Run {unRun :: StateT St IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO)

{- | Run the typed Csound monad to get underlying dynamic Csound file
for rendering to text.
-}
exec :: Options -> Run () -> IO Csd
exec opts act = do
  st <- execStateT (unRun $ setupInstr0 >> act >> setupUdos) initSt
  pure $ st.csd{csdFlags = Options.csdFlags st.options}
  where
    initSt =
      St
        { csd =
            Csd
              { csdFlags = Options.csdFlags opts
              , csdOrc = Orc emptyE []
              , csdSco = Sco (Just foreverTime) [] []
              , csdPlugins = []
              }
        , freshId = initFreshId
        , freshVar = 1
        , clearVars = []
        , isGlobal = True
        , currentRate = Nothing
        , options = opts
        , readInit = ReadInit HashMap.empty
        , ftables = Ftables{ftableCache = FtableMap Map.empty, ftableFreshId = 1}
        }

    foreverTime = 100 * 604800.0

-----------------------------------------------------------------------------
-- basic functions

readIsDebug :: Run IsDebug
readIsDebug = Run $ do
  opts <- gets (.options)
  pure (fromMaybe def opts.csdRender).inferenceOptions.opcodeInferenceDebug

clearVarsAct :: Dep ()
clearVarsAct = do
  vars <- lift $ Run $ gets (.clearVars)
  mapM_ (\v -> writeVar IfKr v 0) vars

{- | Inserts new instrument body.
The instrument identifier is automatically allocated to fresh integer
-}
insertInstr :: E -> Run InstrId
insertInstr expr = do
  isDebug <- readIsDebug
  traceShowIf isDebug (ppE expr) $ do
    freshInstrId <- getFreshInstrId expr
    case freshInstrId of
      InstrExist instrId -> pure instrId
      NewInstr instrId -> do
        modifyCsd $ \csd -> csd{csdOrc = insertOrcInstrument (Instr instrId expr) (csdOrc csd)}
        pure instrId

-- | Inserts new named instrument body.
insertNamedInstr :: Text -> E -> Run InstrId
insertNamedInstr name expr = do
  modifyCsd $ \csd -> csd{csdOrc = insertOrcInstrument (Instr instrId expr) (csdOrc csd)}
  pure instrId
  where
    instrId = InstrLabel name

insertOrcInstrument :: Instr -> Orc -> Orc
insertOrcInstrument instr x = x{orcInstruments = instr : orcInstruments x}

-- sets stIsGlobal to False during the action
localy :: Run a -> Run a
localy (Run act) = Run $ do
  prev <- gets (.isGlobal)
  modify' $ \st -> st{isGlobal = False}
  res <- act
  modify' $ \st -> st{isGlobal = prev}
  pure res

insertGlobalExpr :: E -> Run ()
insertGlobalExpr expr =
  modifyCsd $ \csd -> csd{csdOrc = insertOrc $ csdOrc csd}
  where
    insertOrc x = x{orcHead = noRate $ Seq (toPrimOr $ orcHead x) (toPrimOr expr)}

insertNote :: InstrId -> CsdEvent -> Run ()
insertNote instrId evt =
  modifyCsd $ \csd -> csd{csdSco = insertSco $ csdSco csd}
  where
    insertSco x = x{scoNotes = (instrId, [evt]) : scoNotes x}

setTotalDur :: Double -> Run ()
setTotalDur duration =
  modifyCsd $ \csd -> csd{csdSco = setSco $ csdSco csd}
  where
    setSco x = x{scoTotalDur = Just duration}

initGlobalVar :: Rate -> E -> Run Var
initGlobalVar rate initVal = do
  v <- getFreshGlobalVar rate
  insertGlobalExpr $ initVarExpr v
  pure v
  where
    initVarExpr v = noRate $ InitVar v (toPrimOr initVal)

initClearableGlobalVar :: Rate -> E -> Run Var
initClearableGlobalVar rate initVal = do
  v <- initGlobalVar rate initVal
  Run $ modify' $ \st -> st{clearVars = v : st.clearVars}
  pure v

initGlobalArrVar :: Rate -> [E] -> Run Var
initGlobalArrVar rate sizes = do
  var <- getFreshGlobalVar rate
  insertGlobalExpr $ initVarExpr var
  pure var
  where
    initVarExpr v = noRate $ InitArr v (fmap toPrimOr sizes)

isGlobalInstr :: Run Bool
isGlobalInstr = Run $ gets (.isGlobal)

-- | TODO
saveTabs :: [Gen] -> Run E
saveTabs = error "TODO: tab lists are not defined yet. undefined"

getOptions :: Run Options
getOptions = Run $ gets (.options)

-- | Preambule instrument which sets up global constants and utilities
setupInstr0 :: Run ()
setupInstr0 = do
  opt <- getOptions
  insertGlobalExpr =<< execDepT (instr0 opt)
  where
    instr0 :: Options -> Dep ()
    instr0 opt = do
      globalConstants opt

setupUdos :: Run ()
setupUdos = do
  mUdos <- (.csdUdos) <$> getOptions
  case mUdos of
    Just (Udos udos) -> insertGlobalExpr =<< execDepT (mapM_ (insertUdo <=< readUdo) udos)
    Nothing -> pure ()
  where
    insertUdo :: Text -> Dep ()
    insertUdo udoContent = do
      verbatim $ udoContent <> "\n"

    readUdo :: UdoDef -> Dep Text
    readUdo = \case
      UdoBody text -> pure text
      UdoFile file -> lift $ fmap Text.pack $ readFileWithExistCheck file

-- | Creates expression with global constants
globalConstants :: Options -> Dep ()
globalConstants opt = do
  setSr $ Options.defSampleRate opt
  setKsmps $ Options.defBlockSize opt
  setNchnls $ Options.defNchnls opt
  setZeroDbfs 1
  maybe (return ()) setNchnls_i (Options.csdNchnlsIn opt)
  jackos
  where
    jackos = maybe (return ()) (verbatim . Options.renderJacko) $ Options.csdJacko opt

-----------------------------------------------------------------------------

getReadOnlyVar :: Rate -> E -> Run E
getReadOnlyVar rate expr = Run $ do
  ReadInit initMap <- gets (.readInit)
  readOnlyVar IfIr <$> case HashMap.lookup hash initMap of
    Just vars -> pure $ head vars
    Nothing -> do
      var <- unRun $ initGlobalVar rate expr
      modify' $ \s -> s{readInit = ReadInit $ HashMap.insert hash [var] $ unReadInit s.readInit}
      pure var
  where
    hash = hashE expr

getReadOnlyVars :: [Rate] -> [E] -> Dep [E] -> Run [E]
getReadOnlyVars rates initVals runExpr = do
  fmap (fmap (readOnlyVar IfIr)) $
    case rates of
      [] -> allocProc >> pure []
      [rate] -> fmap pure $ allocSingle rate
      _ -> allocMultiVars
  where
    allocProc :: Run ()
    allocProc = do
      ReadInit initMap <- Run $ gets (.readInit)
      expr <- execDepT runExpr
      let
        hash = hashE expr
      case HashMap.lookup hash initMap of
        Just _ -> pure ()
        Nothing -> do
          insertGlobalExpr expr
          insertReadInit hash []

    allocSingle :: Rate -> Run Var
    allocSingle rate = do
      ReadInit initMap <- Run $ gets (.readInit)
      expr <- execDepT runExpr
      let
        hash = hashE expr
      var <- initGlobalVar rate expr
      insertReadInit hash [var]
      pure var

    allocMultiVars :: Run [Var]
    allocMultiVars = do
      ReadInit initMap <- Run $ gets (.readInit)
      expr <- execDepT runExpr
      let
        hash = hashE expr
      case HashMap.lookup hash initMap of
        Just vars -> pure vars
        Nothing -> do
          vars <- zipWithM initGlobalVar rates initVals
          expr <- execDepT $ zipWithM_ (writeVar IfIr) vars =<< runExpr
          insertGlobalExpr expr
          insertReadInit hash vars
          pure vars

    insertReadInit :: ExpHash -> [Var] -> Run ()
    insertReadInit hash vars =
      Run $ modify' $ \s -> s{readInit = ReadInit $ HashMap.insert hash vars $ unReadInit s.readInit}

-----------------------------------------------------------------------------

getCurrentRate :: Run (Maybe IfRate)
getCurrentRate = Run (gets (.currentRate))

withCurrentRate :: IfRate -> Dep a -> Dep a
withCurrentRate rate act = do
  prevRate <- lift getCurrentRate
  setCurrentRate (Just rate)
  res <- act
  setCurrentRate prevRate
  pure res
  where
    setCurrentRate :: Maybe IfRate -> Dep ()
    setCurrentRate r = lift $ Run $ modify' $ \s -> s{currentRate = r}

-----------------------------------------------------------------------------
-- internal state

-- | Internal state for rendering typed expressions to Csd
data St = St
  { csd :: Csd
  -- ^ Dynamic Csound code
  , freshId :: FreshId
  -- ^ fresh instrument ids
  , freshVar :: Int
  -- ^ fresh names for mutable variables
  , clearVars :: [Var]
  -- ^ variables to clear at th en of the control-cycle
  , isGlobal :: Bool
  -- ^ do we render global or local instrument
  , currentRate :: Maybe IfRate
  -- ^ current rate of execution (Ir or Kr) to distinguish the init phase from the performance phase
  , options :: Options
  -- ^ Csound flags and initial options / settings
  , readInit :: ReadInit
  -- ^ read only global inits that are initialized only once
  , ftables :: Ftables
  }

{- | Global vars that are initialized only once and are read-only
and can be safely cached
-}
newtype ReadInit = ReadInit {unReadInit :: HashMap ExpHash [Var]}

-- | Fresh ids for instruments
data FreshId = FreshId
  { freshIdCounter :: Int
  -- ^ counter for new id
  , freshIdMem :: HashMap ExpHash InstrId
  -- ^ hash map of already defined instruments,
  -- we use it to avoid duplication of defined instruments
  -- here we assme that instrument is unique by hash of it's expression
  -- which is not true, but it's necessary trade-off
  }

initFreshId :: FreshId
initFreshId = FreshId 1 HashMap.empty

modifyCsd :: (Csd -> Csd) -> Run ()
modifyCsd f = Run $ modify' $ \s -> s{csd = f s.csd}

data FreshInstrId
  = InstrExist InstrId
  | NewInstr InstrId

{- | Allocates fresh instrument id.
First it tries to find instr definition in cache by hash.
If it's not present then it allocates fresh id otherwise the old instrument is returned.

Assumption that hash identifies the instrument. It's not safe but we need it for performance
-}
getFreshInstrId :: E -> Run FreshInstrId
getFreshInstrId expr = Run $ do
  instrMem <- gets (freshIdMem . (.freshId))
  case HashMap.lookup hash instrMem of
    Just instrId -> pure $ InstrExist instrId
    Nothing -> do
      freshId <- gets (freshIdCounter . (.freshId))
      let
        instrId = intInstrId freshId
      modify' $ \st -> st{freshId = insertFreshId instrId st.freshId}
      pure $ NewInstr instrId
  where
    hash = hashE expr

    insertFreshId instrId x =
      x
        { freshIdCounter = freshIdCounter x + 1
        , freshIdMem = HashMap.insert hash instrId (freshIdMem x)
        }

getFreshGlobalVar :: Rate -> Run Var
getFreshGlobalVar rate = Run $ do
  varId <- gets (.freshVar)
  modify' $ \st -> st{freshVar = st.freshVar + 1}
  pure $
    Var
      { varType = GlobalVar
      , varRate = rate
      , varName = Text.pack $ show varId
      }

-------------------------------------------------------------------------------------
-- gens

data Ftable
  = GenTable Gen
  | VcoTable VcoInit
  deriving (Eq, Ord)

newtype FtableMap = FtableMap {unFtableMap :: Map Ftable E}

data Ftables = Ftables
  { ftableCache :: FtableMap
  , ftableFreshId :: E
  }

setFreshId :: E -> Ftables -> Ftables
setFreshId freshId x = x{ftableFreshId = freshId}

insertFtableMap :: Ftable -> E -> Ftables -> Ftables
insertFtableMap ft newId x = x{ftableCache = FtableMap $ Map.insert ft newId $ unFtableMap $ ftableCache x}

lookupFtable :: Ftable -> Run (Maybe E)
lookupFtable ft = Run $ do
  FtableMap ftMap <- gets (ftableCache . (.ftables))
  pure $ case ft of
    GenTable _table -> Map.lookup ft ftMap
    VcoTable vcoInit ->
      if Map.member ft ftMap
        then case vcoInit.vcoShape of
          Saw -> Just 0
          Pulse -> Just 2
          Square -> Just 3
          Triangle -> Just 4
          IntegratedSaw -> Just 1
          UserGen gen -> fmap negate $ Map.lookup (GenTable gen) ftMap
        else Nothing

saveGen :: Gen -> Run E
saveGen gen = do
  maybe insertGen pure =<< lookupFtable (GenTable gen)
  where
    insertGen = do
      newId <- getFreshFtableId
      v <- fmap (readOnlyVar IfIr) $ initGlobalVar Ir (ftgen newId gen)
      Run $ modify' $ \st -> st{ftables = insertFtableMap (GenTable gen) v $ setFreshId (v + 1) st.ftables}
      pure v

ftgen :: E -> Gen -> E
ftgen n g =
  opcs "ftgen" [(Ir, rates)] $
    n
      : 0
      : (int $ genSize g)
      : (genIdE $ genId g)
      : ( maybe
            id
            (\file -> (prim (PrimString file) :))
            (genFile g)
            (fmap double $ genArgs g)
        )
  where
    rates = Ir : Ir : Ir : Ir : (maybe id (const (Sr :)) (genFile g) $ (repeat Ir))

genIdE :: GenId -> E
genIdE = \case
  IntGenId n -> int n
  StringGenId a -> prim (PrimString a)

-------------------------------------------------------------------------------------
-- vco

data VcoShape = Saw | Pulse | Square | Triangle | IntegratedSaw | UserGen Gen
  deriving (Eq, Ord)

data VcoInit = VcoInit
  { vcoShape :: VcoShape
  , vcoMul :: Maybe Double
  , vcoMinSize :: Maybe Int
  , vcoMaxSize :: Maybe Int
  }
  deriving (Eq, Ord)

saveVco :: VcoInit -> Run E
saveVco inits =
  maybe insertVco' pure =<< lookupFtable (VcoTable inits)
  where
    insertVco' = do
      (shapeId, mResId) <- vcoShapeId' (vcoShape inits)
      newId <- getFreshFtableId
      nextNewId <- fmap (readOnlyVar IfIr) $ initGlobalVar Ir $ fromVcoInit (isNothing mResId) shapeId newId inits
      Run $ modify' $ \st -> st{ftables = insertFtableMap (VcoTable inits) newId $ setFreshId nextNewId st.ftables}
      pure $ fromMaybe shapeId mResId

    fromVcoInit isGen shapeId newId VcoInit{..} =
      vco2init $ shapeId : newId : maybe 1.05 double vcoMul : defInt vcoMinSize : defInt vcoMaxSize : (if isGen then [negate shapeId] else [])
      where
        defInt = maybe (-1) int

vco2init :: [E] -> E
vco2init = opcs "vco2init" [(Ir, repeat Ir)]

vcoShapeId' :: VcoShape -> Run (E, Maybe E)
vcoShapeId' = \case
  Saw -> simple 1 0
  IntegratedSaw -> simple 2 1
  Pulse -> simple 4 2
  Square -> simple 8 3
  Triangle -> simple 16 4
  UserGen gen -> (,Nothing) . negate <$> saveGen gen
  where
    simple n resId = pure (int n, Just resId)

getFreshFtableId :: Run E
getFreshFtableId = Run $ gets (ftableFreshId . (.ftables))

readFileWithExistCheck :: FilePath -> Run String
readFileWithExistCheck file = liftIO $ do
  isOk <- doesFileExist file
  if isOk
    then readFile file
    else error $ "File to include in Csound file does not exist: " <> file

-------------------------------------------------------------------------------------
-- update options

setOption :: Options -> Run ()
setOption opt = Run $ modify' $ \st -> st{options = opt <> st.options}

setDefaultOption :: Options -> Run ()
setDefaultOption opt = Run $ modify' $ \st -> st{options = st.options <> opt}
