-- | Imperative branching constructs like if-then-else
module Csound.Typed.Core.Types.SE.Logic
  ( when1
  , whens
  , whenD1
  , whenDs
  , untilDo
  , whileDo
  , whileDoD
  , untilDoD
  ) where

import Control.Monad
import Control.Monad.Trans.Class (lift)

import Csound.Dynamic (E, DepT, CodeBlock, IfRate (..))
import Csound.Dynamic qualified as Dynamic
import Csound.Typed.Core.State (Run)
import Csound.Typed.Core.Types.Prim
import Csound.Typed.Core.Types.SE

-- | Constructs generic if-block statement with single then case
-- We can choose constructors for: if, while, until statements
ifBlockBy :: Val cond => (E -> DepT Run (CodeBlock E) -> DepT Run ()) -> cond -> SE () -> SE ()
ifBlockBy cons p body =
  SE $ do
    pE <- lift $ toE p
    cons pE (Dynamic.toBlock $ unSE body)

-- | Invokes the given procedure if the boolean signal is true.
when1 :: BoolSig -> SE () -> SE ()
when1 xp body = case xp of
    PrimBoolSig p -> if p then body else return ()
    _             -> ifBlockBy (Dynamic.when1 IfKr) xp body

-- | The chain of @when1@s. Tests all the conditions in sequence
-- if everything is false it invokes the procedure given in the second argument.
whens :: [(BoolSig, SE ())] -> SE () -> SE ()
whens bodies el = case bodies of
    []   -> el
    _    -> SE $ join $ lift $ do
        checksE <- mapM (toE . fst) bodies
        let bodiesE = fmap (Dynamic.toBlock . unSE . snd) bodies
            elE = Dynamic.toBlock $ unSE el
        pure $ Dynamic.whens IfKr (zip checksE bodiesE) elE

-- | Invokes the given procedure if the boolean signal is true.
whenD1 :: BoolD -> SE () -> SE ()
whenD1 xp body = case xp of
    PrimBoolD p -> if p then body else return ()
    _           -> ifBlockBy (Dynamic.when1 IfIr) xp body

-- | The chain of @when1@s. Tests all the conditions in sequence
-- if everything is false it invokes the procedure given in the second argument.
whenDs :: [(BoolD, SE ())] -> SE () -> SE ()
whenDs bodies el = case bodies of
    []   -> el
    _    -> SE $ join $ lift $ do
        checksE <- mapM (toE . fst) bodies
        let bodiesE = fmap (Dynamic.toBlock . unSE . snd) bodies
            elE = Dynamic.toBlock $ unSE el
        pure $ Dynamic.whens IfIr (zip checksE bodiesE) elE

untilDo :: BoolSig -> SE () -> SE ()
untilDo = ifBlockBy (Dynamic.untilBlock IfKr)

whileDo :: BoolSig -> SE () -> SE ()
whileDo = ifBlockBy (Dynamic.whileBlock IfKr)

untilDoD :: BoolD -> SE () -> SE ()
untilDoD = ifBlockBy (Dynamic.untilBlock IfIr)

whileDoD :: BoolD -> SE () -> SE ()
whileDoD = ifBlockBy (Dynamic.whileBlock IfIr)
