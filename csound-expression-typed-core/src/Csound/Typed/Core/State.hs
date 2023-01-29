-- | Internal state for typed core of the EDSL
module Csound.Typed.Core.State
  ( Run (..)
  , localy
  , exec
  , insertInstr
  , insertNote
  , setTotalDur
  , insertGlobalExpr
  , initGlobalVar
  , isGlobalInstr
  , saveGen
  , saveTabs
  , getOptions
  ) where

import Csound.Dynamic
import Csound.Typed.Core.State.Options (Options)
import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text

newtype Run a = Run { unRun :: StateT St IO a }
  deriving newtype (Functor, Applicative, Monad)

exec :: Options -> Run () -> IO Csd
exec opts (Run act) = stCsd <$> execStateT act initSt
  where
    initSt = St
      { stCsd = Csd
          { csdFlags = mempty
          , csdOrc   = Orc emptyE []
          , csdSco   = Sco Nothing [] []
          , csdPlugins = []
          }
      , stFreshId = initFreshId
      , stFreshVar = 1
      , stIsGlobal = True
      , stGens = Map.empty
      , stOptions = opts
      }

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

isGlobalInstr :: Run Bool
isGlobalInstr = Run $ gets stIsGlobal

saveGen :: Gen -> Run E
saveGen gen = Run $ do
  genMap <- gets stGens
  case Map.lookup gen genMap of
    Just n  -> pure $ int n
    Nothing -> do
      let newId = Map.size genMap + 1
      modify' $ \st -> st { stGens = Map.insert gen newId $ stGens st }
      pure $ int newId

-- | TODO
saveTabs :: [Gen] -> Run E
saveTabs = undefined

getOptions :: Run Options
getOptions = Run $ gets stOptions

-----------------------------------------------------------------------------
-- internal state

-- | Internal state for rendering typed expressions to Csd
data St = St
  { stCsd        :: !Csd
  , stFreshId    :: !FreshId
  , stFreshVar   :: !Int
  , stIsGlobal   :: !Bool
  , stGens       :: !(Map Gen Int)
  , stOptions    :: !Options
  }

data FreshId = FreshId
  { freshIdCounter :: !Int
  , freshIdMem     :: !(HashMap ExpHash InstrId)
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

