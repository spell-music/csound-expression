module Csound.Dynamic.Render (
  RenderOptions (..),
  renderCsd,
  module X,
) where

import Text.PrettyPrint.Leijen.Text qualified as P

import Csound.Dynamic.Render.Instr
import Csound.Dynamic.Render.Pretty
import Csound.Dynamic.Tfm.InferTypes as X (InferenceOptions (..), OpcodeInferenceStrategy (..))
import Csound.Dynamic.Types
import Data.Default
import Data.Text (Text)
import Data.Text.Lazy qualified as Lazy.Text
import Text.PrettyPrint.Leijen.Text qualified as Pretty

data RenderOptions = RenderOptions
  { inferenceOptions :: InferenceOptions
  }
  deriving (Eq, Ord, Show, Read)

instance Default RenderOptions where
  def =
    RenderOptions
      { inferenceOptions = def
      }

renderCsd :: RenderOptions -> Csd -> Text
renderCsd opts a =
  Lazy.Text.toStrict $
    Pretty.displayT $
      Pretty.renderPretty 0.4 80 $
        ppCsdFile
          (renderFlags $ csdFlags a)
          (renderOrc (inferenceOptions opts) $ csdOrc a)
          (renderSco $ csdSco a)
          (csdPlugins a)

renderFlags :: Flags -> Doc
renderFlags = P.pretty

renderOrc :: InferenceOptions -> Orc -> Doc
renderOrc opts a = vcatSep $ headExpr : instrExprs
  where
    headExpr = renderInstrBody opts (orcHead a)
    instrExprs = fmap (renderInstr opts) (orcInstruments a)

renderSco :: Sco -> Doc
renderSco a =
  vcatSep
    [ P.vcat $ fmap (uncurry ppGen) $ scoGens a
    , maybe P.empty ppTotalDur $ scoTotalDur a
    , P.vcat $ fmap (uncurry ppNotes) $ scoNotes a
    ]
