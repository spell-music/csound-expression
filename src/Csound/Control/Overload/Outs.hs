{-# Language 
        TypeFamilies, 
        FlexibleInstances, 
        FlexibleContexts #-}
module Csound.Control.Overload.Outs(
    Outs(..), onArg       
) where

import Csound.Typed

onArg :: Outs b => (a -> b) -> (a -> SE (SigOuts b))
onArg f = toOuts . f

class Sigs (SigOuts a) => Outs a where
    type SigOuts a :: *
    toOuts :: a -> SE (SigOuts a)

instance Outs Sig where
	type SigOuts Sig = Sig
	toOuts = return 

instance Outs (Sig, Sig) where
	type SigOuts (Sig, Sig) = (Sig, Sig)
	toOuts = return

instance Outs (Sig, Sig, Sig, Sig) where
	type SigOuts (Sig, Sig, Sig, Sig) = (Sig, Sig, Sig, Sig)
	toOuts = return

instance Outs (SE Sig) where
	type SigOuts (SE Sig) = Sig
	toOuts = id 

instance Outs (SE (Sig, Sig)) where
	type SigOuts (SE (Sig, Sig)) = (Sig, Sig)
	toOuts = id

instance Outs (SE (Sig, Sig, Sig, Sig)) where
	type SigOuts (SE (Sig, Sig, Sig, Sig)) = (Sig, Sig, Sig, Sig)
	toOuts = id

