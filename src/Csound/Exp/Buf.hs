{-# Language GADTs #-}
module Csound.Exp.Buf where

import Data.List(foldr1)
import Data.Maybe(maybeToList, fromJust)

import Control.Applicative
import Control.Monad(ap)
import Control.Monad.Trans.State

import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper

data BufState = BufState 
    { curTaps :: [E]
    , curRead :: Maybe E
    , curWrite :: Maybe E
    }

newtype Buf a = Buf { unBuf :: State BufState a }

instance Functor Buf where
    fmap f = Buf . fmap f . unBuf

instance Monad Buf where
    return = Buf . return
    ma >>= mf = Buf $ unBuf ma >>= unBuf . mf

delayr :: Double' -> Buf Sig
delayr a = Buf $ state $ \st ->
    let res = withRate Ar $ ExpBuf Delayr (maybeToList $ curWrite st) (unDouble' a)
    in  (Sig res, st{ curTaps = [], curRead = Just res, curWrite = Nothing })

deltap :: Sig -> Buf Sig 
deltap a = Buf $ state $ \st ->
    let res = withRate Ar $ ExpBuf Deltap (maybeToList $ curRead st) (unSig $ kr a)
    in  (Sig res, st{ curTaps = res : curTaps st })

delayw :: Sig -> Buf ()
delayw a = Buf $ state $ \st ->
    let res = withRate Ar $ ExpBuf Delayw (curTaps st) (unSig $ ar a)
    in  ((), st{ curRead = Nothing, curWrite = Just res })

buf :: CsdTuple a => Buf a -> a
buf a = mapCsdTuple (dep $ fromJust $ curWrite st) res
    where (res, st) = runState (unBuf a) (BufState [] Nothing Nothing)
          dep :: E -> E -> E
          dep d a = noRate $ Depends [d] a      


