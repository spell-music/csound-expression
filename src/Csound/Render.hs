module Csound.Render where

import Data.List(transpose)
import Data.Maybe(catMaybes)
import qualified Data.Map as M

import Temporal.Media(eventEnd)
import Control.Monad.Trans.State(evalState)

import Text.PrettyPrint
import Data.Fix

import Csound.Exp
import Csound.Exp.Wrapper hiding (double, int)
import Csound.Tfm.TfmTree(ftableMap)
import Csound.Render.Sco
import Csound.Render.Instr
import Csound.Render.Options
import Csound.Tfm.TfmTree(FtableMap)
import Csound.Exp.Numeric

import Csound.Opcode(clip, zeroDbfs)

csd :: (Out a, Out b) => CsdOptions -> (a -> b) -> [SigOut a] -> String
csd opt globalEffect as = show $ csdFile 
    (renderFlags opt)
    (renderInstr0 nchn (massignTable ids as) opt)
    (vcat $ punctuate newline $ firstInstr : lastInstr : zipWith renderInstr' ids instrs)
    (vcat $ firstInstrNote : lastInstrNote : zipWith (renderScores strs fts) ids scos)
    (renderStringTable strs)
    (renderTotalDur $$ renderFtables fts)
    where scos   = map (scoSigOut' . sigOutContent) as          
          instrs = map (orcSigOut  . sigOutContent) as    
          fts    = ftableMap $ lastInstrExp : instrs
          strs   = stringMap $ concat scos
          ids    = take nInstr [2 .. ]
          nchn   = getNchnls lastInstrExp
          nInstr = length as
          firstInstrId = 1
          lastInstrId  = nInstr + 2
          
          renderInstr' instrId instr = renderInstr (OutInstrPort instrId) fts instrId instr
           
          firstInstr = clearInstr 1 nchn ids
          lastInstr = mixingInstr fts lastInstrId lastInstrExp
          
          lastInstrExp = mixingInstrExp globalEffect ids as
           
          scoSigOut' x = case x of
              PlainSigOut _ _ -> scoSigOut x
              _ -> []            

          dur = maybe 64000000 id $ totalDur as
          renderTotalDur = text "f0" <+> double dur
          firstInstrNote = alwayson firstInstrId dur
          lastInstrNote  = alwayson lastInstrId dur
          alwayson instrId time = char 'i' <> int instrId <+> double 0 <+> double dur

csdFile flags instr0 instrs scores strTable ftables = 
    tag "CsoundSynthesizer" [
        tag "CsOptions" [flags],
        tag "CsInstruments" [
            instr0, strTable, instrs],
        tag "CsScore" [
            ftables, scores]]        


massignTable :: Out a => [Int] -> [SigOut a] -> [Massign]
massignTable ids instrs = catMaybes $ zipWith mk ids instrs
    where mk n instr = case sigOutContent $ instr of
            Midi chn _ -> Just $ Massign chn n
            _ -> Nothing

renderFtables = renderMapTable renderFtableEntry
renderStringTable = renderMapTable renderStringEntry

renderFtableEntry ft id = char 'f' 
    <>  int id 
    <+> int 0 
    <+> (int $ ftableSize ft)
    <+> (int $ ftableGen ft) 
    <+> (hsep $ map double $ ftableArgs ft)
 
renderStringEntry str id = text "strset" <+> int id <> comma <+> (doubleQuotes $ text str)

renderMapTable :: (a -> Int -> Doc) -> M.Map a Int -> Doc
renderMapTable phi = vcat . map (uncurry phi) . M.toList


tag :: String -> [Doc] -> Doc
tag name content = vcat $ punctuate newline [
    char '<' <> text name <> char '>', 
    vcat $ punctuate newline content, 
    text "</" <> text name <> char '>']  

newline = char '\n'


mixingInstr :: FtableMap -> Int -> E -> Doc
mixingInstr fts = renderInstr OutPlain fts

mixingInstrExp :: (Out a, Out b) => (a -> b) -> [Int] -> [SigOut a] -> E
mixingInstrExp globalEffect ids as = 
    sigOut $ globalEffect $ evalState toTuple $ fmap clipSig $ sumSigs $ zipWith applyEffect ids as
    where applyEffect :: Int -> SigOut a -> [E]
          applyEffect instrId a = sigOutEffect a $ globalOuts
            where globalOuts = fmap (gOutVar instrId) [1 ..]
            
          sumSigs :: [[E]] -> [E]  
          sumSigs = fmap sum . transpose   
          
          clipSig :: E -> E
          clipSig x = unSig $ clip (Sig x) 0 zeroDbfs
          
getNchnls :: E -> Int
getNchnls x = case unFix x of
    RatedExp _ (Outs xs) -> length xs
            
        
    

clearInstr :: Int -> Nchnls -> [Int] -> Doc
clearInstr instrId nchnls ids = instrHeader instrId $ vcat $ fmap (clearOuts nchnls) ids
    where clearOuts :: Nchnls -> Int -> Doc
          clearOuts n id = vcat $ fmap (clearStmt . gOut id) [1 .. n]

          clearStmt :: Doc -> Doc  
          clearStmt name = name <+> equals <+> int 0

totalDur :: [SigOut a] -> Maybe Double
totalDur as 
    | null as'  = Nothing
    | otherwise = Just $ maximum $ map eventEnd . scoSigOut =<< as' 
    where as' = filter isNotMidi $ map sigOutContent as
          isNotMidi x = case x of
            Midi _ _ -> False
            _ -> True
  



