module Unparsed where

import Csound.Gen.Types
import Csound.Gen.Parse
import Csound.Gen.Pretty

import Paths_gen_opcodes

main = do
    (_, res) <- parsed
    mapM_ (putStrLn . (\(a, b, c) -> a ++ " " ++ b ++ " " ++ c)) res

parsed :: IO ([Chap], [Unparsed])
parsed = fmap parse . readFile =<< getDataFileName "resources/MiscQuickref.html" 
