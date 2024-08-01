-- | The vital tools.
module Csound.Air (
  -- | The basic sound waves: pure sine, sawtooth, square, triangle, LFOs.
  module Csound.Air.Wave,
  -- | Hard and soft sync
  module Csound.Air.Wave.Sync,
  -- | Envelope generators.
  module Csound.Air.Envelope,
  -- | Filters
  module Csound.Air.Filter,
  -- | Argument modifiers
  module Csound.Air.ModArg,
  -- | Sound file playback
  module Csound.Air.Wav,
  -- | Granular synthesis
  module Csound.Air.Granular,
  module Csound.Air.Granular.Morpheus,
  -- | Spectral Processing
  module Csound.Air.Spec,
  -- | Effects: reverbs, choruses, delays etc.
  module Csound.Air.Fx,
  module Csound.Air.Fx.FxBox,
  -- | Widgets to make live performances.
  module Csound.Air.Live,
  -- | A multitap looper.
  module Csound.Air.Looper,
  -- | Patches.
  module Csound.Air.Patch,
  -- | Shortcuts for Scores.
  module Csound.Air.Sco,
  -- | Scheduling signals with event streams
  module Csound.Air.Seg,
  -- | Triggering sound samples with events, keyboard and midi.
  module Csound.Air.Sampler,
  -- | Advanced panning functions
  module Csound.Air.Pan,
  -- | FM synth
  module Csound.Air.Fm,
  -- | Padsynth
  module Csound.Air.Padsynth,
  -- | Other usefull stuff.
  module Csound.Air.Misc,
) where

import Csound.Air.Envelope
import Csound.Air.Filter
import Csound.Air.Fm
import Csound.Air.Fx
import Csound.Air.Fx.FxBox
import Csound.Air.Granular
import Csound.Air.Granular.Morpheus
import Csound.Air.Live
import Csound.Air.Looper
import Csound.Air.Misc
import Csound.Air.ModArg
import Csound.Air.Padsynth
import Csound.Air.Pan
import Csound.Air.Patch
import Csound.Air.Sampler
import Csound.Air.Sco
import Csound.Air.Seg
import Csound.Air.Spec
import Csound.Air.Wav
import Csound.Air.Wave
import Csound.Air.Wave.Sync
