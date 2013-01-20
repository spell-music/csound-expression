module Csound.Render.Options where

import Data.Default
import Text.PrettyPrint

import Csound.Exp.Wrapper(Channel)

type CtrlId = Int

data CsdOptions = CsdOptions 
    { csdFlags      :: String
    , csdRate       :: Int
    , csdBlockSize  :: Int
    , csdSeed       :: Maybe Int
    , csdInitc7     :: [(Channel, CtrlId, Double)] }

instance Default CsdOptions where
    def = CsdOptions 
            { csdFlags = ""
            , csdRate  = 44100
            , csdBlockSize = 64
            , csdSeed = Nothing
            , csdInitc7 = [] }

renderFlags = text . csdFlags

type Nchnls = Int

data Massign = Massign 
    { massignChannel :: Channel
    , massignInstr   :: Int
    }

type InstrId = Int

renderInstr0 :: Nchnls -> [Massign] -> CsdOptions -> Doc
renderInstr0 nchnls massignTable opt = vcat [
    stmt "sr"    $ csdRate opt,
    stmt "ksmps" $ csdBlockSize opt,
    stmt "nchnls" nchnls,   
    maybe empty seed $ csdSeed opt,    
    vcat $ map initc7 $ csdInitc7 opt,    
    vcat $ fmap renderMassign massignTable]
    where stmt a b = text a <+> equals <+> int b
          seed n = text "seed" <+> int n
          initc7 (chn, ctl, val) = text "initc7" <+> 
              (hsep $ punctuate comma [int chn, int ctl, double val])
            
          newline = char '\n'
  
renderMassign :: Massign -> Doc
renderMassign a = text "massign" 
    <+> (int $ massignChannel a) <> comma <+> (int $ massignInstr a)


