-- | The implementation of the instruments that I've found in the Csound catalog
-- and collection of Csond-algorithmic composition (by Michael Gogins) and
-- some other sources.
--
-- Requirements:
--
-- * Library strives for modularity and simplicity. 
-- 
-- * It must be possible to use all instruments live with midi, so the function 'Csound.Base.idur' is forbidden.
module Csound.Catalog 
    ( module Csound.Catalog.Drum
    , module Csound.Catalog.Effect
    , module Csound.Catalog.Envelope
    , module Csound.Catalog.Reson
    , module Csound.Catalog.Wave    
    ) where 

import Csound.Catalog.Drum
import Csound.Catalog.Effect
import Csound.Catalog.Envelope
import Csound.Catalog.Reson
import Csound.Catalog.Wave

