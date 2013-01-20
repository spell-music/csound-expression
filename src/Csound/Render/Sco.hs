{-# Language 
        TypeSynonymInstances,
        FlexibleInstances #-}
module Csound.Render.Sco where

import Data.List(nub)
import qualified Data.Map as M
import Text.PrettyPrint hiding (render)
import Control.Monad.Trans.State
import Data.Fix

import Temporal.Music.Score(Score, Event(..), Dur, render, alignByZero)

import Csound.Exp
import Csound.Exp.Wrapper hiding (int, double)
import Csound.Tfm.TfmTree(FtableMap)

type InstrId = Int

type StringMap = M.Map String Int

-------------------------------------------------------
-- scores

type Note = [Prim]
data Msg = Msg

effect :: (CsdTuple a, CsdTuple b) => (a -> b) -> SigOut a -> SigOut b
effect f a = a{ sigOutEffect = fromTuple . f . evalState toTuple . sigOutEffect a }

data SigOut a = SigOut 
    { sigOutEffect  :: [E] -> [E]
    , sigOutContent :: PlainSigOut
    } 

data PlainSigOut 
    = PlainSigOut 
      { orcSigOut :: E 
      , scoSigOut :: [Event Dur Note] }
    | Midi 
      { midiChn   :: Channel
      , orcSigOut :: E }
        


class CsdTuple a => Out a

instance Out Sig
instance Out (Sig, Sig)
instance Out (Sig, Sig, Sig, Sig)

sigOut :: Out a => a -> E
sigOut = noRate . Outs . fromTuple

sco :: (Arg a, Out b) => (a -> b) -> Score a -> SigOut b
sco instr scores = SigOut id $ PlainSigOut (sigOut $ instr arg) (fromScore scores)

nchnls :: Out a => SigOut a -> Int
nchnls (SigOut _ e) = case unFix $ orcSigOut e of
    RatedExp _ (Outs xs) -> length xs


fromScore :: Arg a => Score a -> [Event Dur Note]
fromScore a = alignByZero $ render $ fmap toNote a

midi :: Out a => Channel -> (Msg -> a) -> SigOut a
midi n = SigOut id . Midi n . sigOut . ($ Msg)

-----------------------------------------------------------------
-- render

renderScores :: StringMap -> FtableMap -> InstrId -> [Event Dur Note] -> Doc
renderScores strs fts instrId as = vcat $ map (renderNote strs fts instrId) as


renderNote :: StringMap -> FtableMap -> InstrId -> Event Dur Note -> Doc
renderNote strs fts instrId event = char 'i' <> int instrId <+> time <+> dur <+> args
    where time = double $ eventStart event
          dur  = double $ eventDur event
          args = hsep $ map prim $ eventContent event
          prim x = case x of
              PrimInt n -> int n
              PrimDouble d -> double d
              PrimFtable f -> int $ fts M.! f
              PrimString s -> int $ strs M.! s
              

stringMap :: [Event Dur Note] -> StringMap
stringMap as = M.fromList $ zip (nub $ allStrings =<< as) [1 .. ]
    where allStrings evt = primStrings =<< eventContent evt
          primStrings x = case x of
              PrimString s -> [s]
              _ -> []




