module Csound.Gen.Types where

import Data.Char
import Data.Maybe

data PackageType = Typed
  deriving (Show)

data Node a = Node
  { nodeName :: String
  , nodeItems :: [a]
  }
  deriving (Show)

instance Functor Node where
  fmap f node = Node{nodeName = nodeName node, nodeItems = fmap f $ nodeItems node}

filterNode :: (a -> Bool) -> Node a -> Node a
filterNode pred node = node{nodeItems = filter pred $ nodeItems node}

type Chap = Node Sec
type Sec = Node Opc

data Opc = Opc
  { opcName :: String
  , opcSignature :: Signature
  , opcDoc :: OpcDoc
  }
  deriving (Show)

data Signature = Signature
  { rates :: Rates
  , types :: Types
  }
  deriving (Show)

data Rates
  = Single [(Rate, RateList)]
  | Multi RateList RateList
  | Opr1
  | Opr1k
  | InfOpr
  | SingleOpr [(Rate, RateList)]
  deriving (Show)

data RateList = JustList [Rate] | Repeat Rate | Append RateList RateList
  deriving (Show)

data Types = Types
  { inTypes :: InTypes
  , outTypes :: OutTypes
  }
  deriving (Show)

newtype InTypes = InTypes [Type]
  deriving (Show)

data OutTypes
  = OutTuple
  | Tuple
  | TheTuple [Type]
  | SingleOut Type
  | SE OutTypes
  | OutNone
  deriving (Show)

data OpcDoc = OpcDoc
  { opcDocShortDescription :: String
  , opcDocLongDescription :: String
  , opcDocCode :: [String]
  , opcDocLink :: String
  }
  deriving (Show)

-- | Needs transformers package imported
chapNeedTrans :: Chap -> Bool
chapNeedTrans ch = any opcNeedTrans $ nodeItems =<< nodeItems ch
  where
    opcNeedTrans opc = case opcType opc of
      DirtySingle -> True
      Procedure -> True
      _ -> False

-- | Needs proxy module imported
chapNeedProxy :: Chap -> Bool
chapNeedProxy ch = any opcNeedProxy $ nodeItems =<< nodeItems ch
  where
    opcNeedProxy opc =
      case outTypes $ types $ opcSignature opc of
        SE Tuple -> True
        SE OutTuple -> True
        _ -> False

-- | Needs proxy module imported
chapNeedMonad :: Chap -> Bool
chapNeedMonad ch = any opcNeedMonad $ nodeItems =<< nodeItems ch
  where
    opcNeedMonad opc = isSE
      where
        isSE = case outTypes $ types $ opcSignature opc of
          SE _ -> True
          OutNone -> True
          _ -> False

data Rate = Xr | Ar | Kr | Ir | Sr | Fr | Wr | Tvar
  deriving (Show, Eq)

data Type = Sig | D | Tab | Str | Spec | Wspec | Sf | TvarType | Msg | TypeList Type | SigOrD
  deriving (Show, Eq)

data OpcType
  = PureSingle
  | DirtySingle
  | Procedure
  | PureMulti
  | DirtyMulti
  deriving (Show, Eq)

isConstant :: Opc -> Bool
isConstant = isConst . rates . opcSignature
  where
    isConst x = case x of
      Single rs -> all (isNull . snd) rs
      Multi _ ins -> isNull ins
      _ -> False

    isNull x = case x of
      JustList [] -> True
      _ -> False

opcType :: Opc -> OpcType
opcType a = case outTypes $ types $ opcSignature a of
  SingleOut _ -> PureSingle
  SE (SingleOut _) -> DirtySingle
  OutNone -> Procedure
  SE OutNone -> Procedure
  x | isTuple x -> PureMulti
  SE x | isTuple x -> DirtyMulti
  where
    isTuple x = case x of
      OutTuple -> True
      Tuple -> True
      TheTuple _ -> True
      _ -> False

getSingleRateList :: RateList -> Maybe Rate
getSingleRateList x = case x of
  JustList [a] -> Just a
  _ -> Nothing

isSingleRateList :: RateList -> Bool
isSingleRateList = isJust . getSingleRateList

isEmptyRateList :: RateList -> Bool
isEmptyRateList x = case x of
  JustList [] -> True
  _ -> False

longerRateList :: RateList -> RateList -> Ordering
longerRateList x y = case (x, y) of
  (JustList a, JustList b) -> length a `compare` length b
  (JustList _, _) -> LT
  (_, JustList _) -> GT
  _ -> EQ

isDirty :: Opc -> Bool
isDirty x = case opcType x of
  DirtySingle -> True
  Procedure -> True
  DirtyMulti -> True
  _ -> False

isPure :: Opc -> Bool
isPure = not . isDirty

isSingle :: Opc -> Bool
isSingle x = case opcType x of
  PureSingle -> True
  DirtySingle -> True
  _ -> False

isProcedure :: Opc -> Bool
isProcedure x = case opcType x of
  Procedure -> True
  _ -> False

isMulti :: Opc -> Bool
isMulti x = case opcType x of
  PureMulti -> True
  DirtyMulti -> True
  _ -> False

packageName = "csound-expression-opcodes"

resourceCabalFileName packageType = cabalFileName packageType ++ "-template"

cabalFileName packageType = case packageType of
  Typed -> "csound-expression-opcodes.cabal"

libCabalFileName packageType = packageName ++ "/" ++ cabalFileName packageType

fullModuleName packageType name = "Csound." ++ show packageType ++ ".Opcode." ++ name
fullPath packageType name = packageName ++ "/src/Csound/" ++ show packageType ++ "/Opcode/" ++ name ++ ".hs"
mainModuleName packageType = "Csound." ++ show packageType ++ ".Opcode"
mainModulePath packageType = packageName ++ "/src/Csound/" ++ show packageType ++ "/Opcode.hs"

----------------------------------------------------------

firstLower, firstUpper :: String -> String
firstUpper x = toUpper (head x) : tail x
firstLower x = toLower (head x) : tail x

allOpcs :: Chap -> [Opc]
allOpcs = (nodeItems =<<) . nodeItems
