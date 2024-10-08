module Csound.Dynamic.Render.Instr (
  renderInstr,
  renderInstrBody,
) where

import Control.Arrow (second)
import Control.Monad.Trans.State.Strict

import Data.Fix (Fix (..), foldFix)
import Data.Fix.Cse (cse {-cseFramed, FrameInfo(..)-}, fromDag)

import Text.PrettyPrint.Leijen.Text qualified as P

import Csound.Dynamic.Tfm.IfBlocks
import Csound.Dynamic.Tfm.InferTypes (InferenceOptions)
import Csound.Dynamic.Tfm.InferTypes qualified as Infer
import Csound.Dynamic.Tfm.Liveness
import Csound.Dynamic.Tfm.TmpVars
import Csound.Dynamic.Tfm.UnfoldMultiOuts

import Csound.Dynamic.Debug
import Csound.Dynamic.Render.Pretty
import Csound.Dynamic.Types hiding (Var)

type Dag f = [(Int, f Int)]

renderInstr :: InferenceOptions -> Instr -> Doc
renderInstr opts a = ppInstr (instrName a) $ renderInstrBody opts (instrBody a)

renderInstrBody :: InferenceOptions -> E -> Doc
renderInstrBody opts a
  | null dag = P.empty
  | otherwise = render dag
  where
    dag = toDag a
    render = P.vcat . flip evalState 0 . mapM (uncurry ppStmt . clearEmptyResults) . collectRates opts . removeTmpVars . (\p -> traceIf opts.opcodeInferenceDebug (ppDag p) p)

-------------------------------------------------------------
-- E -> Dag

toDag :: E -> Dag RatedExp
toDag expr = fromDag $ cse $ trimByArgLength expr

trimByArgLength :: E -> E
trimByArgLength = foldFix $ \x -> Fix x{ratedExpExp = phi $ ratedExpExp x}
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
collectRates opts dag = fmap (second ratedExpExp) res4
  where
    res4 = liveness lastFreshId3 res3
    (res3, lastFreshId3) = unfoldMultiOuts inferRes2
    inferRes2 = inferRes1{Infer.typedProgram = filterDepCases $ Infer.typedProgram inferRes1}
    inferRes1 = collectIfBlocks inferRes
    inferRes =
      Infer.inferTypes opts $
        fmap (uncurry Infer.Stmt) $
          (\a -> traceIf opts.opcodeInferenceDebug (ppDag a) a)
            dag

ppDag :: Dag RatedExp -> String
ppDag a =
  unlines
    [ "DAG"
    , unlines $
        fmap
          ( \(ls, rs) ->
              unwords
                [ show ls
                , "="
                , show $ fmap (either show show . unPrimOr) $ ratedExpExp rs
                , maybe "" (("| rate: " <>) . show) (ratedExpRate rs)
                ]
          )
          a
    ]

-----------------------------------------------------------
-- Dag -> Dag

filterDepCases :: [Infer.Stmt Infer.Var] -> [Infer.Stmt Infer.Var]
filterDepCases = filter (not . isDepCase . Infer.stmtRhs)
  where
    isDepCase x = case ratedExpExp x of
      Starts -> True
      Seq _ _ -> True
      Ends _ -> True
      _ -> False
