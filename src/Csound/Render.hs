module Csound.Render(
    renderCsd, renderCsdBy, out, outs
) where

import Data.Default
import Data.Maybe(catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.Trans.State(evalState)

import Text.PrettyPrint
import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper hiding (double, int)
import Csound.Tfm.TfmTree(tabMap)
import Csound.Render.Sco
import Csound.Render.Instr
import Csound.Render.Options
import Csound.Tfm.TfmTree(TabMap)
import Csound.Exp.Numeric

import Csound.Opcode(clip, zeroDbfs)

-- | Synonym for @return . return@.
out :: Sig -> Out
out = return . return

-- | Synonym for @return@.
outs :: [Sig] -> Out
outs = return

-- | Renders Csound file.
renderCsd :: [SigOut] -> String
renderCsd = renderCsdBy def

-- | Renders Csound file with options.
renderCsdBy :: CsdOptions -> [SigOut] -> String
renderCsdBy opt as = show $ csdFile 
    (renderFlags opt)
    (renderInstr0 (nchnls lastInstrExp) (midiAssignTable ids as) opt)
    (vcat $ punctuate newline $ firstInstr : lastInstr : zipWith (renderInstr krateSet fts) ids instrs)
    (vcat $ firstInstrNote : lastInstrNote : zipWith (renderScores strs fts) ids scos)
    (renderStringTable strs)
    (renderTotalDur $$ renderTabs fts)
    where scos   = map (scoSigOut' . sigOutContent) as          
          (instrs, effects, initOuts) = unzip3 $ zipWith runExpReader as ids    
          fts    = tabMap $ lastInstrExp : instrs
          strs   = stringMap $ concat scos
          ids    = take nInstr [2 .. ]
          
          nInstr = length as
          firstInstrId = 1
          lastInstrId  = nInstr + 2          
           
          firstInstr = renderInstr krateSet fts firstInstrId $ execSE $ sequence_ initOuts
          lastInstr  = renderInstr krateSet fts lastInstrId lastInstrExp
          
          lastInstrExp = mixingInstrExp globalEffect effects
           
          scoSigOut' x = case x of
              PlainSigOut _ _ -> scoSigOut x
              _ -> []            

          dur = maybe 64000000 id $ totalDur as
          renderTotalDur = text "f0" <+> double dur
          firstInstrNote = alwayson firstInstrId dur
          lastInstrNote  = alwayson lastInstrId dur
          alwayson instrId time = char 'i' <> int instrId <+> double 0 <+> double dur
          krateSet = S.fromList $ csdKrate opt
          globalEffect = csdEffect opt

csdFile flags instr0 instrs scores strTable tabs = 
    tag "CsoundSynthesizer" [
        tag "CsOptions" [flags],
        tag "CsInstruments" [
            instr0, strTable, instrs],
        tag "CsScore" [
            tabs, scores]]        


midiAssignTable :: [Int] -> [SigOut] -> [MidiAssign]
midiAssignTable ids instrs = catMaybes $ zipWith mk ids instrs
    where mk n instr = case sigOutContent $ instr of
            Midi ty chn _ -> Just $ MidiAssign ty chn n
            _ -> Nothing

renderTabs = renderMapTable renderTabEntry
renderStringTable = renderMapTable renderStringEntry

renderTabEntry ft id = char 'f' 
    <>  int id 
    <+> int 0 
    <+> (int $ tabSize ft)
    <+> (int $ tabGen ft) 
    <+> (hsep $ map double $ tabArgs ft)
 
renderStringEntry str id = text "strset" <+> int id <> comma <+> (doubleQuotes $ text str)

renderMapTable :: (a -> Int -> Doc) -> M.Map a Int -> Doc
renderMapTable phi = vcat . map (uncurry phi) . M.toList


tag :: String -> [Doc] -> Doc
tag name content = vcat $ punctuate newline [
    char '<' <> text name <> char '>', 
    vcat $ punctuate newline content, 
    text "</" <> text name <> char '>']  

newline = char '\n'


mixingInstrExp :: ([[Sig]] -> SE [Sig]) -> [SE [Sig]] -> E
mixingInstrExp globalEffect effects = execSE $ outs' . fmap clip' =<< globalEffect =<< sequence effects
    where clip' x = clip x 0 zeroDbfs
          
totalDur :: [SigOut] -> Maybe Double
totalDur as 
    | null as'  = Nothing
    | otherwise = Just $ maximum $ map eventEnd . scoSigOut =<< as' 
    where as' = filter isNotMidi $ map sigOutContent as
          isNotMidi x = case x of
            Midi _ _ _ -> False
            _ -> True
  



