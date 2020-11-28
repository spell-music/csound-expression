module Csound.Typed.Plugins.ZeroDelayConvolution(
	 ZConvSpec(..), zconv, zconv'
) where

import Data.Boolean
import Data.Default
import Control.Monad.Trans.Class
import Control.Applicative

import Csound.Dynamic

import Csound.Typed.Types
import Csound.Typed.GlobalState
import qualified Csound.Typed.GlobalState.Elements as E(zeroDelayConvolutionPlugin)


-- | Zero convolution specification
data ZConvSpec = ZConvSpec
    { zconvPartSize :: D -- ^ first partition size in samples
    , zconvRatio    :: D -- ^ partition growth ratio
    , zconvNp       :: D -- ^ total number of partition sizes
    }

instance Default ZConvSpec where
    def = ZConvSpec 64 4 6

-------------------------------------------------------------------------------

-- | Zero delay convolution with default parameters.
--
-- > zconv tabIR  ain = ...
zconv :: Tab -> Sig -> Sig
zconv = zconv' def

-- | zero delay convolution.
--
-- > zconv' (ZConvSpec ipart irat inp) ifn ain
--
-- Original UDO code by Victor Lazzarini.
--
-- /**************************************************
-- asig ZConv ain,ipart,irat,inp,ifn
-- ain - input signal
-- ipart - first partition size in samples
-- irat - partition growth ratio
-- inp - total number of partition sizes
-- ifn - function table number containing the IR
-- **************************************************/
zconv' :: ZConvSpec -> Tab -> Sig -> Sig
zconv' (ZConvSpec ipart irat inp) ifn ain = fromGE $ do
    addUdoPlugin E.zeroDelayConvolutionPlugin
    f <$> toGE ain <*> toGE ipart <*> toGE irat <*> toGE inp <*> toGE ifn
    where f ain ipart irat inp ifn = opcs "ZConv" [(Ar, [Ar, Ir, Ir, Ir, Ir])] [ain, ipart, irat, inp, ifn]
