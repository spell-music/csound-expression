module Csound.Dynamic.Render(
    renderCsd
) where

import qualified Text.PrettyPrint.Leijen as P

import Csound.Dynamic.Render.Instr
import Csound.Dynamic.Render.Pretty
import Csound.Dynamic.Types

renderCsd :: Csd -> String
renderCsd a = show $ ppCsdFile
    (renderFlags $ csdFlags a)
    (renderOrc $ csdOrc a)
    (renderSco   $ csdSco a)
    (csdPlugins a)

renderFlags :: Flags -> Doc
renderFlags = P.pretty

renderOrc :: Orc -> Doc
renderOrc a = vcatSep $ headExpr : instrExprs
    where
        headExpr    = renderInstrBody (orcHead a)
        instrExprs  = fmap renderInstr (orcInstruments a)

renderSco :: Sco -> Doc
renderSco a = vcatSep
    [ P.vcat $ fmap (uncurry ppGen)   $ scoGens a
    , maybe P.empty ppTotalDur $ scoTotalDur a
    , P.vcat $ fmap (uncurry ppNotes) $ scoNotes a ]

