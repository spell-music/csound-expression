module Unparsed where

import Csound.Gen.Parse
import Csound.Gen.Pretty
import Csound.Gen.Types

import Paths_gen_csound_opcodes

main = do
  (_, res) <- parsed
  mapM_ (putStrLn . (\(a, b, c) -> a ++ " " ++ b ++ " " ++ c)) res

parsed :: IO ([Chap], [Unparsed])
parsed = fmap parse . readFile =<< getDataFileName "resources/MiscQuickref.html"
