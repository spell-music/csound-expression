module Prepare where

import qualified Data.Vector as V
import System.ProgressBar
import Text.Printf

import Csound.Gen.Parse
import Csound.Gen.Types
import Paths_gen_csound_opcodes

main = writeFile "docs.txt" . show =<< getDocs =<< readFile =<< getDataFileName "resources/MiscQuickref.html"

getDocs :: String -> IO [(String, (String, String))]
getDocs quickRef = do
  progRef <- newProgressBar defStyle 40 (Progress 0 (fromIntegral $ V.length nameVec) ())
  docs <- mapM (download progRef) opcs
  return $ zip names docs
  where
    names = fmap opcName opcs
    nameVec = V.fromList names
    (parsed, unparsed) = parse quickRef
    opcs = allOpcs =<< parsed
    download progRef x = do
      res <- downloadDesc x
      incProgress progRef 1
      return res

    showOpcode = fmt . maybe "" id . (nameVec V.!?) . fromIntegral . progressDone
      where
        fmt :: String -> String
        fmt x = printf "% 14s" x
