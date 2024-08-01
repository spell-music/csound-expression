{-# LANGUAGE InstanceSigs #-}

-- | With ports we can communicate non-constant values between instruments
module Csound.Core.Types.SE.Port (
  Port (..),
  newPort,
) where

import Control.Monad
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Data.Text qualified as Text

import Csound.Core.Render.Options (UdoDef (..), addUdo)
import Csound.Core.State (Dep)
import Csound.Core.Types.Prim.D
import Csound.Core.Types.Prim.Val
import Csound.Core.Types.SE.Core
import Csound.Core.Types.Tuple
import Csound.Dynamic (E, Rate (..))
import Csound.Dynamic qualified as Dynamic

-- https://flossmanual.csound.com/csound-language/local-and-global-variables#the-chn-opcodes-for-global-variables

-- | With ports we can send audio and control signals between running instruments
newtype Port a = Port {unPort :: D}
  deriving newtype (IsPrim, Val, FromTuple, Tuple, Arg)

chnUpdateUdo :: Text
chnUpdateUdo =
  Text.unlines
    [ "giPort init 1"
    , "opcode " <> chnUpdateOpcodeName <> ", i, 0"
    , "xout giPort"
    , "giPort = giPort + 1"
    , "endop"
    ]

chnUpdateOpcodeName :: Text
chnUpdateOpcodeName = "GetFreePort"

newPort :: SE (Port a)
newPort =
  fmap (withOption $ addUdo chnUpdateOpcodeName (UdoBody chnUpdateUdo)) $
    liftOpcDep chnUpdateOpcodeName [(Ir, [])] ()

instance IsRef Port where
  readRef pid =
    fmap (toTuple . pure) $
      SE $
        zipWithM (\rate name -> chnget rate name) (getRates pid) =<< getNames pid

  writeRef = writeBy chnset id

  writeInitRef = error "TODO: define me with chnseti"
  readInitRef = error "TODO: define me with chnget restricted to Irate"

  mixRef = writeBy chnmix id

  clearRef pid = SE $ mapM_ chnclear =<< getNames pid

writeBy :: forall a. (Tuple a) => (Rate -> E -> E -> Dep ()) -> (Rate -> Rate) -> Port a -> a -> SE ()
writeBy set toRate pid val = SE $ do
  valE <- lift (fromTuple val)
  names <- getNames pid
  zipWithM_ (\(rate, name) v -> set (toRate rate) name v) (zip (getRates pid) names) valE

--------------------------------------------------------------
-- utils

getNames :: forall a. (Tuple a) => Port a -> Dep [E]
getNames pid = do
  pidE <- lift (toE pid)
  pure $ toPortName pidE <$> [1 .. tupleArity @a]

getRates :: forall a. (Tuple a) => Port a -> [Rate]
getRates _ = tupleRates @a

toPortName :: E -> Int -> E
toPortName chnId name =
  sprintf formatString [chnId]
  where
    formatString = Dynamic.str $ 'p' : show name ++ "_" ++ "%d"

--------------------------------------------------------------
-- opcodes

chnset :: Rate -> E -> E -> Dep ()
chnset rate name value = Dynamic.depT_ $ Dynamic.opcs "chnset" [(Xr, [rate, Sr])] [value, name]

chnmix :: Rate -> E -> E -> Dep ()
chnmix rate name value = Dynamic.depT_ $ Dynamic.opcs "chnmix" [(Xr, [rate, Sr])] [value, name]

chnget :: Rate -> E -> Dep E
chnget rate name = Dynamic.opcsDep "chnget" [(rate, [Sr])] [name]

chnclear :: E -> Dep ()
chnclear name = Dynamic.depT_ $ Dynamic.opcs "chnclear" [(Xr, [Sr])] [name]

sprintf :: E -> [E] -> E
sprintf a as = Dynamic.opcs "sprintf" [(Sr, Sr : repeat Ir)] (a : as)
