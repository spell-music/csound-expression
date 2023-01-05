module Csound.Dynamic.Render.Instr(
  renderInstr,
  renderInstrBody
) where

import Control.Arrow(second)
import Control.Monad.Trans.State.Strict
import Data.Default

import Data.Fix(Fix(..), foldFix)
import Data.Fix.Cse(fromDag, cseFramed, FrameInfo(..))

import qualified Text.PrettyPrint.Leijen.Text as P

import Csound.Dynamic.Tfm.InferTypes (InferenceOptions)
import Csound.Dynamic.Tfm.InferTypes qualified as Infer
import Csound.Dynamic.Tfm.UnfoldMultiOuts
import Csound.Dynamic.Tfm.Liveness
import Csound.Dynamic.Tfm.SaturateIf (saturateIf)

import Csound.Dynamic.Types hiding (Var)
import Csound.Dynamic.Render.Pretty

type Dag f = [(Int, f Int)]

renderInstr :: InferenceOptions -> Instr -> Doc
renderInstr opts a = ppInstr (instrName a) $ renderInstrBody opts (instrBody a)

renderInstrBody :: InferenceOptions -> E -> Doc
renderInstrBody opts a
  | null dag  = P.empty
  | otherwise = render dag
    where
      dag = toDag a
      render = P.vcat . flip evalState 0 . mapM (uncurry ppStmt . clearEmptyResults) . collectRates opts

-------------------------------------------------------------
-- E -> Dag

toDag :: E -> Dag RatedExp
toDag expr = filterDepCases $ fromDag $ cseFramed getFrameInfo $ trimByArgLength expr

getFrameInfo :: RatedExp a -> FrameInfo
getFrameInfo x = case ratedExpExp x of
    -- Imperative If-then-else
    IfBegin _ _   -> StartFrame
--     ElseIfBegin _ -> NextFrame
    ElseBegin     -> NextFrame
    IfEnd         -> StopFrame
    -- looping constructions
    UntilBegin _ _ -> StartFrame
    UntilEnd     -> StopFrame
    WhileBegin _ _ -> StartFrame
    WhileRefBegin _ -> StartFrame
    WhileEnd     -> StopFrame
    _            -> NoFrame


trimByArgLength :: E -> E
trimByArgLength = foldFix $ \x -> Fix x{ ratedExpExp = phi $ ratedExpExp x }
  where
    phi x = case x of
      Tfm info xs -> Tfm (info{infoSignature = trimInfo (infoSignature info) xs}) xs
      _ -> x

    trimInfo signature args = case signature of
      SingleRate tab -> SingleRate $ fmap trim tab
      MultiRate outs ins -> MultiRate outs (trim ins)
      where
        trim = take (length args)

clearEmptyResults :: ([Infer.Var], Exp Infer.Var) -> ([Infer.Var], Exp Infer.Var)
clearEmptyResults (res, expr) = (filter ((/= Xr) . Infer.varType) res, expr)

collectRates :: InferenceOptions -> Dag RatedExp -> [([Infer.Var], Exp Infer.Var)]
collectRates opts dag = fmap (second ratedExpExp) res3
  where
    res3 = liveness lastFreshId1 res2

    res2 =
      if Infer.programHasIfs inferRes
        then saturateIf def res1
        else res1

    (res1, lastFreshId1) = unfoldMultiOuts inferRes
    inferRes = Infer.inferTypes opts $ fmap (uncurry Infer.Stmt) dag

-----------------------------------------------------------
-- Dag -> Dag

filterDepCases :: Dag RatedExp -> Dag RatedExp
filterDepCases = filter (not . isDepCase . snd)
  where isDepCase x = case ratedExpExp x of
          Starts  -> True
          Seq _ _ -> True
          Ends _  -> True
          _       -> False
