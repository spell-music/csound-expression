module Csound.Dynamic.Render(
  RenderOptions (..),
  renderCsd,
  module X
) where

import qualified Text.PrettyPrint.Leijen.Text as P

import Csound.Dynamic.Render.Instr
import Csound.Dynamic.Render.Pretty
import Csound.Dynamic.Types
import Csound.Dynamic.Tfm.InferTypes as X (InferenceOptions (..), OpcodeInferenceStrategy (..))
import Data.Default

data RenderOptions = RenderOptions
  { inferenceOptions :: !InferenceOptions
  }
  deriving (Eq, Ord, Show, Read)

instance Default RenderOptions where
  def = RenderOptions
          { inferenceOptions = def
          }

renderCsd :: RenderOptions -> Csd -> String
renderCsd opts a = show $ ppCsdFile
    (renderFlags $ csdFlags a)
    (renderOrc (inferenceOptions opts) $ csdOrc a)
    (renderSco   $ csdSco a)
    (csdPlugins a)

renderFlags :: Flags -> Doc
renderFlags = P.pretty

renderOrc :: InferenceOptions -> Orc -> Doc
renderOrc opts a = vcatSep $ headExpr : instrExprs
  where
    headExpr    = renderInstrBody opts (orcHead a)
    instrExprs  = fmap (renderInstr opts) (orcInstruments a)

renderSco :: Sco -> Doc
renderSco a = vcatSep
    [ P.vcat $ fmap (uncurry ppGen) $ scoGens a
    , maybe P.empty ppTotalDur $ scoTotalDur a
    , P.vcat $ fmap (uncurry ppNotes) $ scoNotes a ]

