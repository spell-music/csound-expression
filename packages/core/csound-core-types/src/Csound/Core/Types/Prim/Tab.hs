{- | Csound tables, which are creted in the score section and referenced
by numbers in the Csound code. In the haskell library this process is automated
numbers are assigned automatically.
-}
module Csound.Core.Types.Prim.Tab (
  Tab (..),
  unTab,
  PreTab (..),
  preTab,
  preStringTab,
  TabSize (..),
  TabArgs (..),
  updateTabSize,
  renderTab,
  fromPreTab,
  getPreTabUnsafe,
  skipNorm,
  skipNormPreTab,
  forceNorm,
  nsamp,
  ftlen,
  ftchnls,
  ftsr,
  ftcps,
  TabList,
  tabList,
  fromTabList,
  fromTabListD,
) where

import Data.Default
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as Text

import Csound.Core.Render.Options (TabFi (..), defTabFi)
import Csound.Core.State (Run)
import Csound.Core.State qualified as State
import Csound.Core.Types.Prim.D
import Csound.Core.Types.Prim.Sig
import Csound.Core.Types.Prim.Val
import Csound.Dynamic (E, Gen (..), GenId (..), Rate (..))
import Csound.Dynamic qualified as D

-- | Tables (or arrays)
data Tab
  = Tab (Run E)
  | TabPre PreTab

unTab :: Tab -> Run E
unTab = toE

renderTab :: Tab -> Run E
renderTab x = case x of
  TabPre a -> State.saveGen =<< fromPreTab a
  Tab _ -> error "table should be primitive"

instance Val Tab where
  fromE = Tab

  toE = \case
    Tab a -> a
    TabPre a -> renderPreTab a

  valRate = Ir

preTab :: TabSize -> Int -> TabArgs -> PreTab
preTab size gen args = PreTab size (IntGenId gen) args

preStringTab :: TabSize -> Text -> TabArgs -> PreTab
preStringTab size gen args = PreTab size (StringGenId gen) args

data PreTab = PreTab
  { preTabSize :: TabSize
  , preTabGen :: GenId
  , preTabArgs :: TabArgs
  }

-- Table size.
data TabSize
  = -- Size is fixed by the user.
    SizePlain Int
  | -- Size is relative to the renderer settings.
    SizeDegree
      { hasGuardPoint :: Bool
      , sizeDegree :: Int -- is the power of two
      }

instance Default TabSize where
  def =
    SizeDegree
      { hasGuardPoint = False
      , sizeDegree = 0
      }

-- Table arguments can be
data TabArgs
  = -- absolute
    ArgsPlain (Int -> [Double])
  | {-    -- or relative to the table size (used for tables that implement interpolation)
        | ArgsRelative [Double]
        -- GEN 16 uses unusual interpolation scheme, so we need a special case
        | ArgsGen16 [Double] -}
    FileAccess String [Double]

renderPreTab :: PreTab -> Run E
renderPreTab a = State.saveGen =<< fromPreTab a

getPreTabUnsafe :: String -> Tab -> PreTab
getPreTabUnsafe msg x = case x of
  TabPre a -> a
  _ -> error msg

fromPreTab :: PreTab -> Run Gen
fromPreTab a = (\opt -> go (defTabFi opt) a) <$> State.getOptions
  where
    go :: TabFi -> PreTab -> Gen
    go tabFi tab = Gen size (preTabGen tab) args (Text.pack <$> file)
      where
        size = defineTabSize (getTabSizeBase tabFi tab) (preTabSize tab)
        (args, file) = defineTabArgs size (preTabArgs tab)

getTabSizeBase :: TabFi -> PreTab -> Int
getTabSizeBase tf tab =
  case preTabGen tab of
    IntGenId intId -> IM.findWithDefault (tabFiBase tf) intId (tabFiGens tf)
    StringGenId stringId -> M.findWithDefault (tabFiBase tf) stringId (tabNamedFiGens tf)

defineTabSize :: Int -> TabSize -> Int
defineTabSize base x =
  case x of
    SizePlain n -> n
    SizeDegree guardPoint degree -> byGuardPoint guardPoint $ byDegree base degree
  where
    byGuardPoint guardPoint
      | guardPoint = (+ 1)
      | otherwise = id

    byDegree zero n = 2 ^ max 0 (zero + n)

defineTabArgs :: Int -> TabArgs -> ([Double], Maybe String)
defineTabArgs size args = case args of
  ArgsPlain as -> (as size, Nothing)
  FileAccess filename as -> (as, Just filename)

-- | Skips normalization (sets table size to negative value)
skipNorm :: Tab -> Tab
skipNorm x = case x of
  Tab _ -> error "you can skip normalization only for primitive tables (made with gen-routines)"
  TabPre a -> TabPre $ skipNormPreTab a

-- | Skips normalization (sets table size to negative value)
skipNormPreTab :: PreTab -> PreTab
skipNormPreTab a = a{preTabGen = skipNormGenId $ preTabGen a}

skipNormGenId :: GenId -> GenId
skipNormGenId = mapIntGenId (negate . abs)

{- | Force normalization (sets table size to positive value).
Might be useful to restore normalization for table 'Csound.Tab.doubles'.
-}
forceNorm :: Tab -> Tab
forceNorm x = case x of
  Tab _ -> error "you can force normalization only for primitive tables (made with gen-routines)"
  TabPre a -> TabPre $ a{preTabGen = normGenId $ preTabGen a}

normGenId :: GenId -> GenId
normGenId = mapIntGenId abs

mapIntGenId :: (Int -> Int) -> GenId -> GenId
mapIntGenId f gid = case gid of
  IntGenId intId -> IntGenId (f intId)
  _ -> gid

----------------------------------------------------------------------------
-- change table size

updateTabSize :: (TabSize -> TabSize) -> Tab -> Tab
updateTabSize phi x = case x of
  Tab _ -> error "you can change size only for primitive tables (made with gen-routines)"
  TabPre a -> TabPre $ a{preTabSize = phi $ preTabSize a}

----------------------------------------------------------------------------
-- Tab of tabs

-- | Container list of tables
data TabList = TabList {unTabList :: Run E}

instance Val TabList where
  fromE = TabList
  toE = unTabList
  valRate = Ir

tabList :: [Tab] -> TabList
tabList xs = TabList $ State.saveTabs =<< mapM fromPreTab (getPreTabs xs)
  where
    getPreTabs = \case
      [] -> []
      Tab _ : as -> getPreTabs as
      TabPre a : as -> a : getPreTabs as

fromTabList :: TabList -> Sig -> Tab
fromTabList ts knd = Tab $ do
  ets <- toE ts
  eknd <- toE knd
  return $ tableK eknd ets

fromTabListD :: TabList -> D -> Tab
fromTabListD ts ind = Tab $ do
  ets <- toE ts
  eind <- toE ind
  return $ tableI eind ets

-----------------------------------------------------------
-- read tables

tableK :: E -> E -> E
tableK a1 a2 = D.opcs "table" [(Kr, [Kr, Ir])] [a1, a2]

tableI :: E -> E -> E
tableI a1 a2 = D.opcs "table" [(Ir, [Ir, Ir])] [a1, a2]

----------------------------------------------

{- | nsamp — Returns the number of samples loaded into a stored function table number.

> nsamp(x) (init-rate args only)

csound doc: <http://www.csounds.com/manual/html/nsamp.html>
-}
nsamp :: Tab -> D
nsamp = liftE $ D.opr1 "nsamp"

-- | Returns a length of the table.
ftlen :: Tab -> D
ftlen = liftE $ D.opr1 "ftlen"

-- | Returns the number of channels for a table that stores wav files
ftchnls :: Tab -> D
ftchnls = liftE $ D.opr1 "ftchnls"

-- | Returns the sample rate for a table that stores wav files
ftsr :: Tab -> D
ftsr = liftE $ D.opr1 "ftsr"

-- | Returns the base frequency for a table that stores wav files
ftcps :: Tab -> D
ftcps = liftE $ D.opr1 "ftcps"
