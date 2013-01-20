module Csound.Exp.Random where

import Data.List(unfoldr)

import Csound.Exp
import Csound.Exp.Wrapper

--------------------------------------------
-- random signals (means to duplicate expression)

se :: Val a => SE a -> (a, SE a)
se a = (wrap $ unwrap $ a, wrap $ onExp phi $ unwrap a)
    where phi x = case x of
            Tfm info xs -> Tfm info{ infoNextSE = setNextId $ infoNextSE info } xs
            _ -> x
          setNextId = maybe (Just 0) (Just . succ)

se2 :: Val a => SE a -> (a, a, SE a)
se2 s0 = (a, b, s2)
    where (a, s1) = se s0
          (b, s2) = se s1  

ses :: Val a => SE a -> [a]
ses = unfoldr $ \a -> Just (se a)

