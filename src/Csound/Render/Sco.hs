{-# Language 
        TypeSynonymInstances,
        FlexibleInstances #-}
module Csound.Render.Sco where

import Data.List(nub)
import Data.Tuple(swap)
import qualified Data.Map as M
import Text.PrettyPrint hiding (render)
import Control.Monad.Trans.State
import Control.Monad((<=<), zipWithM)
import Data.Fix

import Temporal.Music.Score(Score, Event(..), Dur, render, alignByZero)

import Csound.Exp
import Csound.Exp.Wrapper hiding (int, double)
import Csound.Tfm.TfmTree(TabMap)
import Csound.Exp.Cons(opcs)
import Csound.Exp.Numeric

type InstrId = Int

type StringMap = M.Map String Int

-------------------------------------------------------
-- scores

type Note = [Prim]
data Msg = Msg

effect :: ([Sig] -> SE [Sig]) -> SigOut -> SigOut
effect f a = a{ sigOutEffect = f <=< sigOutEffect a }

data SigOut = SigOut 
    { sigOutEffect  :: [Sig] -> SE [Sig]    
    , sigOutContent :: PlainSigOut
    } 

type ExpReader = Int -> (E, [Var])

runExpReader :: SigOut -> Int -> (E, SE [Sig], SE ())
runExpReader a n = (exp, sigOutEffect a $ fmap readVar vars, mapM_ (flip writeVar (0 :: Sig)) vars)
    where (exp, vars) = (orcSigOut $ sigOutContent a) n


data PlainSigOut 
    = PlainSigOut 
      { orcSigOut :: ExpReader
      , scoSigOut :: [Event Dur Note] }
    | Midi 
      { midiType  :: MidiType
      , midiChn   :: Channel
      , orcSigOut :: ExpReader }
        
data MidiType = Massign | Pgmassign (Maybe Int)


outs :: [Sig] -> SE ()
outs as = se_ $ opcs (name as) [(Xr, repeat Ar)] as
    where name as
            | length as == 1 = "out"
            | otherwise      = "outs"


sco :: (Arg a) => (a -> SE [Sig]) -> Score a -> SigOut
sco instr scores = SigOut return $ PlainSigOut (expReader $ instr arg) (fromScore scores)


expReader :: SE [Sig] -> ExpReader
expReader instr instrId = swap $ runSE $ do             
    as <- instr
    let vars = instrPorts instrId as    
    zipWithM writeVar vars as
    return vars
            

instrPorts :: Int -> [Sig] -> [Var]
instrPorts instrId sigs = fmap (gOutVar instrId) ids
    where ids = fmap fst $ zip [1 ..] sigs

nchnls :: E -> Int
nchnls x = case ratedExpExp $ unFix x of
    Tfm _ as -> length as

fromScore :: Arg a => Score a -> [Event Dur Note]
fromScore a = alignByZero $ render $ fmap toNote a


massign :: Channel -> (Msg -> SE [Sig]) -> SigOut 
massign = midiAssign Massign

pgmassign :: Maybe Channel -> Int -> (Msg -> SE [Sig]) -> SigOut 
pgmassign chn = midiAssign (Pgmassign chn)

midiAssign :: MidiType -> Channel -> (Msg -> SE [Sig]) -> SigOut
midiAssign ty n = SigOut return . Midi ty n . expReader . ($ Msg)



-----------------------------------------------------------------
-- render

renderScores :: StringMap -> TabMap -> InstrId -> [Event Dur Note] -> Doc
renderScores strs fts instrId as = vcat $ map (renderNote strs fts instrId) as


renderNote :: StringMap -> TabMap -> InstrId -> Event Dur Note -> Doc
renderNote strs fts instrId event = char 'i' <> int instrId <+> time <+> dur <+> args
    where time = double $ eventStart event
          dur  = double $ eventDur event
          args = hsep $ map prim $ eventContent event
          prim x = case x of
              PrimInt n -> int n
              PrimDouble d -> double d
              PrimTab f -> int $ fts M.! f
              PrimString s -> int $ strs M.! s
              

stringMap :: [Event Dur Note] -> StringMap
stringMap as = M.fromList $ zip (nub $ allStrings =<< as) [1 .. ]
    where allStrings evt = primStrings =<< eventContent evt
          primStrings x = case x of
              PrimString s -> [s]
              _ -> []




