module Csound.Render.Sco(
    score, SigOut(..), effect, 
    Msg, massign , pgmassign, MidiType(..),

    Event(..), eventEnd, PlainSigOut(..), renderScores,
    runExpReader, nchnls, stringMap, outs'
) where

import Data.List(nub)
import Data.Tuple(swap)
import qualified Data.Map as M
import Control.Monad.Trans.State
import Control.Monad((<=<), zipWithM)
import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper hiding (int, double)
import Csound.Exp.Cons(opcs)
import Csound.Exp.Numeric

import Csound.Render.Pretty

type InstrId = Int

type StringMap = M.Map String Int

-------------------------------------------------------
-- scores

type Note = [Prim]

-- | Midi messages.
data Msg = Msg

data Event a = Event 
    { eventStart :: Double
    , eventDur   :: Double
    , eventContent :: a }
    
eventEnd e = eventStart e + eventDur e

instance Functor Event where
    fmap f a = a{ eventContent = f $ eventContent a }

-- | Applies a global effect function to the signal. With this function we can add reverb or panning to the mixed signal.
-- The argument function takes a list of signals. Each cell of the list contains a signal on the given channel.
effect :: ([Sig] -> Out) -> SigOut -> SigOut
effect f a = a{ sigOutEffect = f <=< sigOutEffect a }

-- | The abstract type of musical tracks. 
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
      , scoSigOut :: [Event Note] }
    | Midi 
      { midiType  :: MidiType
      , midiChn   :: Channel
      , orcSigOut :: ExpReader }
        
data MidiType = Massign | Pgmassign (Maybe Int)


outs' :: [Sig] -> SE ()
outs' as = se_ $ opcs (name as) [(Xr, repeat Ar)] as
    where name as
            | length as == 1 = "out"
            | otherwise      = "outs"


score :: (Arg a) => (a -> Out) -> [(Double, Double, a)] -> SigOut
score instr scores = SigOut return $ 
    PlainSigOut (expReader $ instr toArg) (fmap (\(a, b, c) -> Event a b (toNote argMethods c)) scores)


expReader :: SE [Sig] -> ExpReader
expReader instr instrId = swap $ runSE $ do             
    as <- instr
    let vars = instrPorts instrId as    
    zipWithM (\v a -> writeVar v $ readVar v + a) vars as
    return vars
            

instrPorts :: Int -> [Sig] -> [Var]
instrPorts instrId sigs = fmap (gOutVar instrId) ids
    where ids = fmap fst $ zip [1 ..] sigs

nchnls :: E -> Int
nchnls x = case ratedExpExp $ unFix x of
    Tfm _ as -> length as

massign :: Channel -> (Msg -> Out) -> SigOut 
massign = midiAssign Massign

pgmassign :: Maybe Channel -> Int -> (Msg -> Out) -> SigOut 
pgmassign chn = midiAssign (Pgmassign chn)

midiAssign :: MidiType -> Channel -> (Msg -> SE [Sig]) -> SigOut
midiAssign ty n = SigOut return . Midi ty n . expReader . ($ Msg)

-----------------------------------------------------------------
-- render

renderScores :: StringMap -> TabMap -> InstrId -> [Event Note] -> Doc
renderScores strs fts instrId as = ppScore $ map (renderNote strs fts instrId) as

renderNote :: StringMap -> TabMap -> InstrId -> Event Note -> Doc
renderNote strs fts instrId e = ppNote instrId (eventStart e) (eventDur e) (map prim $ eventContent e)
    where prim x = case x of
              PrimInt n -> int n
              PrimDouble d -> double d
              PrimTab f -> int $ fts M.! f
              PrimString s -> int $ strs M.! s
              

stringMap :: [Event Note] -> StringMap
stringMap as = M.fromList $ zip (nub $ allStrings =<< as) [1 .. ]
    where allStrings evt = primStrings =<< eventContent evt
          primStrings x = case x of
              PrimString s -> [s]
              _ -> []




