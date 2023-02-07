-- | Internal state for typed core of the EDSL
module Csound.Typed.Core.State
  ( Run (..)
  , Dep
  , localy
  , exec
  , insertInstr
  , insertNote
  , setTotalDur
  , insertGlobalExpr
  , initGlobalVar
  , initGlobalArrVar
  , isGlobalInstr
  , saveGen
  , saveTabs
  , getOptions
  , getFreshPort
  ) where

import Control.Monad.IO.Class

import Csound.Dynamic
import Csound.Typed.Core.State.Options (Options)
import Csound.Typed.Core.State.Options qualified as Options
import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

type Dep a = DepT Run a

-- | Monad for typed Csound expressions
newtype Run a = Run { unRun :: StateT St IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Run the typed Csound monad to get underlying dynamic Csound file
-- for rendering to text.
exec :: Options -> Run () -> IO Csd
exec opts act = stCsd <$> execStateT (unRun $ setupInstr0 opts >> act) initSt
  where
    initSt = St
      { stCsd = Csd
          { csdFlags = Options.csdFlags opts
          , csdOrc   = Orc emptyE []
          , csdSco   = Sco (Just foreverTime) [] []
          , csdPlugins = []
          }
      , stFreshId = initFreshId
      , stFreshVar = 1
      , stIsGlobal = True
      , stGens = Map.empty
      , stOptions = opts
      }

    foreverTime = 100 * 604800.0



-----------------------------------------------------------------------------
-- basic functions

-- | Inserts new instrument body.
-- The instrument identifier is automatically allocated to fresh integer
insertInstr :: E -> Run InstrId
insertInstr expr = do
  freshInstrId <- getFreshInstrId expr
  case freshInstrId of
    InstrExist instrId -> pure instrId
    NewInstr instrId -> do
      modifyCsd $ \csd -> csd { csdOrc = insertOrc (Instr instrId expr) (csdOrc csd) }
      pure instrId
  where
    insertOrc instr x = x { orcInstruments = instr : orcInstruments x }

-- sets stIsGlobal to False during the action
localy :: Run a -> Run a
localy (Run act) = Run $ do
  modify' $ \st -> st { stIsGlobal = False }
  res <- act
  modify' $ \st -> st { stIsGlobal = True }
  pure res

insertGlobalExpr :: E -> Run ()
insertGlobalExpr expr =
  modifyCsd $ \csd -> csd { csdOrc = insertOrc $ csdOrc csd }
  where
    insertOrc x = x { orcHead = noRate $ Seq (toPrimOr $ orcHead x) (toPrimOr expr) }

insertNote :: InstrId -> CsdEvent -> Run ()
insertNote instrId evt =
  modifyCsd $ \csd -> csd { csdSco = insertSco $ csdSco csd }
  where
    insertSco x = x { scoNotes = (instrId, [evt]) : scoNotes x }

setTotalDur :: Double -> Run ()
setTotalDur duration =
  modifyCsd $ \csd -> csd { csdSco = setSco $ csdSco csd }
  where
    setSco x = x { scoTotalDur = Just duration }

initGlobalVar :: Rate -> E -> Run Var
initGlobalVar rate initVal = do
  v <- getFreshGlobalVar rate
  insertGlobalExpr $ initVarExpr v
  pure v
  where
    initVarExpr v = noRate $ InitVar v (toPrimOr initVal)

initGlobalArrVar :: Rate -> [E] -> Run Var
initGlobalArrVar rate sizes = do
  var <- getFreshGlobalVar rate
  insertGlobalExpr $ initVarExpr var
  pure var
  where
    initVarExpr v = noRate $ InitArr v (fmap toPrimOr sizes)

isGlobalInstr :: Run Bool
isGlobalInstr = Run $ gets stIsGlobal

saveGen :: Gen -> Run Int
saveGen gen = Run $ do
  genMap <- gets stGens
  case Map.lookup gen genMap of
    Just n  -> pure n
    Nothing -> do
      let newId = Map.size genMap + 1
      modify' $ \st -> st
        { stGens = Map.insert gen newId $ stGens st
        , stCsd = insertTabToSco newId (stCsd st)
        }
      pure newId
  where
    insertTabToSco newId x = x { csdSco = insertSco $ csdSco x }
      where
        insertSco s = s { scoGens = (newId, gen) : scoGens s }

-- | TODO
saveTabs :: [Gen] -> Run E
saveTabs = undefined

getOptions :: Run Options
getOptions = Run $ gets stOptions

-- | Preambule instrument which sets up global constants and utilities
setupInstr0 :: Options -> Run ()
setupInstr0 opt = insertGlobalExpr =<< execDepT instr0
  where
    instr0 :: Dep ()
    instr0 = do
      globalConstants opt
      chnUpdateUdo

-- | Creates expression with global constants
globalConstants :: Options -> Dep ()
globalConstants opt = do
  setSr       $ Options.defSampleRate opt
  setKsmps    $ Options.defBlockSize opt
  setNchnls   $ Options.defNchnls opt
  setZeroDbfs 1
  maybe (return ()) setNchnls_i  (Options.csdNchnlsIn opt)
  jackos
  where
    jackos = maybe (return ()) (verbatim . Options.renderJacko) $ Options.csdJacko opt

-----------------------------------------------------------------------------

chnUpdateUdo :: Dep ()
chnUpdateUdo = verbatim $ Text.unlines [
    "giPort init 1",
    "opcode " <> chnUpdateOpcodeName <> ", i, 0",
    "xout giPort",
    "giPort = giPort + 1",
    "endop"]

chnUpdateOpcodeName :: Text
chnUpdateOpcodeName = "FreePort"

getFreshPort :: Dep E
getFreshPort = depT $ opcs chnUpdateOpcodeName [(Ir, [])] []

-----------------------------------------------------------------------------
-- internal state

-- | Internal state for rendering typed expressions to Csd
data St = St
  { stCsd        :: !Csd               -- ^ Dynamic Csound code
  , stFreshId    :: !FreshId           -- ^ fresh instrument ids
  , stFreshVar   :: !Int               -- ^ fresh names for mutable variables
  , stIsGlobal   :: !Bool              -- ^ do we render global or local instrument
  , stGens       :: !(Map Gen Int)     -- ^ map of Gen-tables to generate unique integer identifiers
  , stOptions    :: !Options           -- ^ Csound flags and initial options / settings
  }

-- | Fresh ids for instruments
data FreshId = FreshId
  { freshIdCounter :: !Int
      -- ^ counter for new id
  , freshIdMem     :: !(HashMap ExpHash InstrId)
      -- ^ hash map of already defined instruments,
      -- we use it to avoid duplication of defined instruments
      -- here we assme that instrument is unique by hash of it's expression
      -- which is not true, but it's necessary trade-off
  }

initFreshId :: FreshId
initFreshId = FreshId 1 HashMap.empty

modifyCsd :: (Csd -> Csd) -> Run ()
modifyCsd f = Run $ modify' $ \s -> s { stCsd = f (stCsd s) }

data FreshInstrId
  = InstrExist InstrId
  | NewInstr InstrId

-- | Allocates fresh instrument id.
-- First it tries to find instr definition in cache by hash.
-- If it's not present then it allocates fresh id otherwise the old instrument is returned.
--
-- Assumption that hash identifies the instrument. It's not safe but we need it for performance
getFreshInstrId :: E -> Run FreshInstrId
getFreshInstrId expr = Run $ do
  instrMem <- gets (freshIdMem . stFreshId)
  case HashMap.lookup hash instrMem of
    Just instrId -> pure $ InstrExist instrId
    Nothing      -> do
      freshId <- gets (freshIdCounter . stFreshId)
      let instrId = intInstrId freshId
      modify' $ \st -> st { stFreshId = insertFreshId instrId $ stFreshId st }
      pure $ NewInstr instrId
  where
    hash = hashE expr

    insertFreshId instrId x = x
      { freshIdCounter = freshIdCounter x + 1
      , freshIdMem = HashMap.insert hash instrId (freshIdMem x)
      }

getFreshGlobalVar :: Rate -> Run Var
getFreshGlobalVar rate = Run $ do
  varId <- gets stFreshVar
  modify' $ \st -> st { stFreshVar = stFreshVar st + 1 }
  pure $ Var
    { varType = GlobalVar
    , varRate = rate
    , varName = Text.pack $ show varId
    }

