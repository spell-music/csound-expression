module Csound.Dynamic.Render.Instr(
    renderInstr, renderInstrBody
) where

import Control.Arrow(second)
import Control.Monad.Trans.State.Strict
import Data.List(sort, find)
import qualified Data.Map as M

import Data.Maybe(fromJust)
import Data.Fix(Fix(..), cata)
import Data.Fix.Cse(fromDag, cseFramed, FrameInfo(..))

import qualified Text.PrettyPrint.Leijen as P

import Csound.Dynamic.Tfm.DeduceTypes
import Csound.Dynamic.Tfm.UnfoldMultiOuts
import Csound.Dynamic.Tfm.Liveness

import Csound.Dynamic.Types hiding (Var)
import Csound.Dynamic.Build(getRates, isMultiOutSignature)
import Csound.Dynamic.Render.Pretty

type Dag f = [(Int, f Int)]

renderInstr :: Instr -> Doc
renderInstr a = ppInstr (instrName a) $ renderInstrBody (instrBody a)

renderInstrBody :: E -> Doc
renderInstrBody a
  | null dag  = P.empty
  | otherwise = render dag
    where
      dag = toDag a
      render = P.vcat . flip evalState 0 . mapM (uncurry ppStmt . clearEmptyResults) . collectRates

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
    UntilBegin _ -> StartFrame
    UntilEnd     -> StopFrame
    WhileBegin _ -> StartFrame
    WhileRefBegin _ -> StartFrame
    WhileEnd     -> StopFrame
    _            -> NoFrame


trimByArgLength :: E -> E
trimByArgLength = cata $ \x -> Fix x{ ratedExpExp = phi $ ratedExpExp x }
    where phi x = case x of
            Tfm info xs -> Tfm (info{infoSignature = trimInfo (infoSignature info) xs}) xs
            _ -> x
          trimInfo signature args = case signature of
            SingleRate tab -> SingleRate $ fmap trim tab
            MultiRate outs ins -> MultiRate outs (trim ins)
            where trim = take (length args)

clearEmptyResults :: ([RatedVar], Exp RatedVar) -> ([RatedVar], Exp RatedVar)
clearEmptyResults (res, expr) = (filter ((/= Xr) . ratedVarRate) res, expr)

collectRates :: Dag RatedExp -> [([RatedVar], Exp RatedVar)]
collectRates dag = fmap (second ratedExpExp) res2
    where res2 = liveness lastFreshId1 res1
          (res1, lastFreshId1)= unfoldMultiOuts unfoldSpec lastFreshId dag1
          (dag1, lastFreshId) = rateGraph dag

-----------------------------------------------------------
-- Dag -> Dag

filterDepCases :: Dag RatedExp -> Dag RatedExp
filterDepCases = filter (not . isDepCase . snd)
  where isDepCase x = case ratedExpExp x of
          Starts  -> True
          Seq _ _ -> True
          Ends _  -> True
          _       -> False

-----------------------------------------------------------
-- deduces types

rateGraph :: [Stmt RatedExp Int] -> ([Stmt RatedExp (Var Rate)], Int)
rateGraph dag = (stmts, lastId)
     where (stmts, lastId) = deduceTypes algSpec dag
           algSpec = TypeGraph mkConvert' defineType'

           mkConvert' a = (to, RatedExp Nothing Nothing $
                   ConvertRate (ratedVarRate to) (ratedVarRate from) $ PrimOr $ Right from)
               where from = convertFrom a
                     to   = convertTo   a

           defineType' (outVar, expr) desiredRates = (ratesForConversion, (outVar', expr'))
               where possibleRate = deduceRate desiredRates expr
                     ratesForConversion = filter (not . flip coherentRates possibleRate) desiredRates
                     expr' = RatedExp Nothing Nothing $ rateExp possibleRate $ ratedExpExp expr
                     outVar' = ratedVar possibleRate outVar

----------------------------------------------------------
-- unfolds multiple rates

unfoldSpec :: UnfoldMultiOuts RatedExp Rate
unfoldSpec = UnfoldMultiOuts getSelector' getParentTypes'
    where getSelector' x = case ratedExpExp x of
                Select _ order (PrimOr (Right parent)) -> Just $ Selector parent order
                _ -> Nothing
          getParentTypes' x = case ratedExpExp x of
                Tfm i _ -> if (isMultiOutSignature $ infoSignature i)
                           then Just (getRates $ ratedExpExp x)
                           else Nothing
                _ -> Nothing

coherentRates :: Rate -> Rate -> Bool
coherentRates to from = case (to, from) of
    (a, b)  | a == b    -> True
    (Xr, _)             -> True
    (Kr, Ir)            -> True
    _                   -> False

deduceRate :: [Rate] -> RatedExp Int -> Rate
deduceRate desiredRates expr = case ratedExpExp expr of
    ExpPrim _ -> case desiredRates of
        [Sr] -> Sr
        _ -> Ir

    Tfm info _ -> case infoSignature info of
        MultiRate _ _ -> Xr
        SingleRate tab ->
            let r1 = tfmNoRate (infoName info) desiredRates tab
            in  case ratedExpRate expr of
                    Just r | M.member r tab -> r
                    Just _ -> r1
                    Nothing -> r1

    ExpNum _ -> case ratedExpRate expr of
        Just r  -> r
        Nothing -> case maximum (Ar : desiredRates) of
            Xr -> Ar
            r -> r

    Select rate _ _ -> rate
    If _ _ _ -> case head $ sort desiredRates of
        Xr -> Ar
        r  -> r
    ReadVar v -> varRate v
    ReadArr v _ -> varRate v
    ReadMacrosString _ -> Sr
    ReadMacrosDouble _ -> Ir
    ReadMacrosInt _ -> Ir
    _  -> Xr
    where tfmNoRate name rates tab = case sort rates of
              [Xr]  -> tfmNoRate name [Ar] tab
              Xr:as -> tfmNoRate name as tab
              as | any (== Ir) as  -> fromJust $ find (flip M.member tab) (Ir : as ++ [minBound .. maxBound])
              as -> fromJust $ find (flip M.member tab) (as ++ [minBound .. maxBound])

rateExp :: Rate -> Exp Int -> Exp RatedVar
rateExp curRate expr = case expr of
    ExpPrim (P n) | curRate == Sr -> ExpPrim (PString n)
    Tfm i xs -> Tfm i $ mergeWithPrimOr (ratesFromSignature curRate (infoSignature i)) xs
    Select rate pid a -> Select rate pid (fmap (ratedVar Xr) a)
    If p t e -> If (rec2 condRate p) (rec1 curRate t) (rec1 curRate e)
    ExpNum _ -> rec2 curRate expr

    ReadVar v -> ReadVar v
    WriteVar v a -> WriteVar v $ rec1 (varRate v) a
    InitVar v a -> InitVar v $ rec1 Ir a -- rec1 (varRate v) a

    ReadArr v as -> ReadArr v $ arrIndex v as
    WriteArr v as b -> WriteArr v (arrIndex v as) (rec1 (varRate v) b)
    WriteInitArr v as b -> WriteInitArr v (arrIndex v as) (rec1 Ir b)
    InitArr v as -> InitArr v $ fmap (rec1 Ir) as
    TfmArr isInit v i xs -> TfmArr isInit v i $ mergeWithPrimOr (ratesFromSignature curRate (infoSignature i)) xs

    ExpPrim p -> ExpPrim p
    IfBegin rootRate _ -> rec2 rootRate expr
    UntilBegin _ -> rec2 condRate expr
    WhileBegin _ -> rec2 condRate expr
    WhileRefBegin var -> WhileRefBegin var
--    ElseIfBegin _ -> rec2 condRate expr
    ElseBegin -> ElseBegin
    IfEnd -> IfEnd
    UntilEnd -> UntilEnd
    WhileEnd -> WhileEnd
    EmptyExp -> EmptyExp
    Verbatim a -> Verbatim a
    InitMacrosString name initValue -> InitMacrosString name initValue
    InitMacrosDouble name initValue -> InitMacrosDouble name initValue
    ReadMacrosString name -> ReadMacrosString name
    ReadMacrosDouble name -> ReadMacrosDouble name
    ReadMacrosInt name -> ReadMacrosInt name
    ExpBool _           -> error $ msg "ExpBool expression should be substituted"
    ConvertRate _ _ _   -> error $ msg "ConvertRate couldn't be here. It's introduced on the later stages of processing"
    where ratesFromSignature rate signature = case signature of
              SingleRate table -> table M.! rate
              MultiRate _ rs   -> rs

          condRate :: Rate
          condRate = max Kr curRate -- Kr

          rec2 r = fmap (fmap (ratedVar r))
          rec1 r = fmap (ratedVar r)

          arrIndex v as = fmap (rec1 (arrIndexVarRate v)) as

          msg txt = "Csound.Dynamic.Render.Instr.rateExp: " ++ txt

arrIndexVarRate v = case varRate v of
    Ir -> Ir
    _  -> Kr

mergeWithPrimOr :: [Rate] -> [PrimOr Int] -> [PrimOr (Var Rate)]
mergeWithPrimOr = zipWith phi
    where
        phi r (PrimOr x) = PrimOr $ case x of
            Left  p -> Left $ updateVarTargetRate r p
            Right n -> Right $ ratedVar r n
        updateVarTargetRate r p = case p of
            PrimVar _ v -> PrimVar r v
            _           -> p


