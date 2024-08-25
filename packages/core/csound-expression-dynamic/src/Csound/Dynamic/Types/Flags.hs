{-# LANGUAGE CPP #-}

-- | Csound's command line flags. See original documentation for detailed overview: <http://www.csounds.com/manual/html/CommandFlagsCategory.html>
module Csound.Dynamic.Types.Flags (
  Flags (..),

  -- * Audio file output
  AudioFileOutput (..),
  FormatHeader (..),
  FormatSamples (..),
  FormatType (..),
  Dither (..),
  IdTags (..),

  -- * Realtime Audio Input/Output
  Rtaudio (..),
  PulseAudio (..),

  -- * MIDI File Input/Ouput
  MidiIO (..),

  -- * MIDI Realtime Input/Ouput
  MidiRT (..),
  Rtmidi (..),

  -- * Display
  Displays (..),
  DisplayMode (..),

  -- * Performance Configuration and Control
  Config (..),
) where

import Control.Applicative

import Data.Char
import Data.Default
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as Text

import Text.PrettyPrint.Leijen.Text

mappendBool :: Bool -> Bool -> Bool
mappendBool a b = getAny $ mappend (Any a) (Any b)

data Flags = Flags
  { audioFileOutput :: AudioFileOutput
  , idTags :: IdTags
  , rtaudio :: Maybe Rtaudio
  , pulseAudio :: Maybe PulseAudio
  , midiIO :: MidiIO
  , midiRT :: MidiRT
  , rtmidi :: Maybe Rtmidi
  , displays :: Displays
  , config :: Config
  , flagsVerbatim :: Maybe Text
  }
  deriving (Eq, Show, Read)

instance Default Flags where
  def = Flags def def def def def def def def def def

#if MIN_VERSION_base(4,11,0)
instance Semigroup Flags where
  x <> y          = x `mappendFlags` y

instance Monoid Flags where
    mempty  = def
#else

instance Monoid Flags where
    mempty  = def
    mappend = mappendFlags

#endif

mappendFlags :: Flags -> Flags -> Flags
mappendFlags a b =
  Flags
    { audioFileOutput = mappend (audioFileOutput a) (audioFileOutput b)
    , idTags = mappend (idTags a) (idTags b)
    , rtaudio = rtaudio a <|> rtaudio b
    , pulseAudio = pulseAudio a <|> pulseAudio b
    , midiIO = mappend (midiIO a) (midiIO b)
    , midiRT = mappend (midiRT a) (midiRT b)
    , rtmidi = rtmidi a <|> rtmidi b
    , displays = mappend (displays a) (displays b)
    , config = mappend (config a) (config b)
    , flagsVerbatim = mappend (flagsVerbatim a) (flagsVerbatim b)
    }

-- Audio file output

data AudioFileOutput = AudioFileOutput
  { formatSamples :: Maybe FormatSamples
  , formatType :: Maybe FormatType
  , output :: Maybe Text
  , input :: Maybe Text
  , nosound :: Bool
  , nopeaks :: Bool
  , dither :: Maybe Dither
  }
  deriving (Eq, Show, Read)

instance Default AudioFileOutput where
  def = AudioFileOutput def def def def False False def

#if MIN_VERSION_base(4,11,0)
instance Semigroup AudioFileOutput where
  x <> y          = x `mappendAudioFileOutput` y

instance Monoid AudioFileOutput where
    mempty  = def

#else

instance Monoid AudioFileOutput where
    mempty  = def
    mappend = mappendAudioFileOutput

#endif

mappendAudioFileOutput :: AudioFileOutput -> AudioFileOutput -> AudioFileOutput
mappendAudioFileOutput a b =
  AudioFileOutput
    { formatSamples = formatSamples a <|> formatSamples b
    , formatType = formatType a <|> formatType b
    , output = output a <|> output b
    , input = input a <|> input b
    , nosound = mappendBool (nosound a) (nosound b)
    , nopeaks = mappendBool (nopeaks a) (nopeaks b)
    , dither = dither a <|> dither b
    }

data FormatHeader = NoHeader | RewriteHeader
  deriving (Eq, Show, Read)

data FormatSamples
  = Bit24
  | Alaw
  | Uchar
  | Schar
  | FloatSamples
  | Ulaw
  | Short
  | Long
  deriving (Eq, Show, Read)

data Dither = Triangular | Uniform
  deriving (Eq, Show, Read)

data FormatType
  = Aiff
  | Au
  | Avr
  | Caf
  | Flac
  | Htk
  | Ircam
  | Mat4
  | Mat5
  | Nis
  | Paf
  | Pvf
  | Raw
  | Sd2
  | Sds
  | Svx
  | Voc
  | W64
  | Wav
  | Wavex
  | Xi
  deriving (Eq, Show, Read)

-- Output file id tags

data IdTags = IdTags
  { idArtist :: Maybe Text
  , idComment :: Maybe Text
  , idCopyright :: Maybe Text
  , idDate :: Maybe Text
  , idSoftware :: Maybe Text
  , idTitle :: Maybe Text
  }
  deriving (Eq, Show, Read)

instance Default IdTags where
  def = IdTags def def def def def def

#if MIN_VERSION_base(4,11,0)
instance Semigroup IdTags where
  x <> y          = x `mappendIdTags` y

instance Monoid IdTags where
    mempty  = def

#else

instance Monoid IdTags where
    mempty  = def
    mappend = mappendIdTags

#endif

mappendIdTags :: IdTags -> IdTags -> IdTags
mappendIdTags a b =
  IdTags
    { idArtist = idArtist a <|> idArtist b
    , idComment = idComment a <|> idComment b
    , idCopyright = idCopyright a <|> idCopyright b
    , idDate = idDate a <|> idDate b
    , idSoftware = idSoftware a <|> idSoftware b
    , idTitle = idTitle a <|> idTitle b
    }

-- Realtime Audio Input/Output

data Rtaudio
  = PortAudio
  | Alsa
  | Jack
      { jackClient :: Text
      , jackInport :: Text
      , jackOutport :: Text
      }
  | Mme
  | CoreAudio
  | NoRtaudio
  deriving (Eq, Show, Read)

data PulseAudio = PulseAudio
  { paServer :: Text
  , paOutput :: Text
  , paInput :: Text
  }
  deriving (Eq, Show, Read)

-- MIDI File Input/Ouput

data MidiIO = MidiIO
  { midiFile :: Maybe Text
  , midiOutFile :: Maybe Text
  , muteTracks :: Maybe Text
  , rawControllerMode :: Bool
  , terminateOnMidi :: Bool
  }
  deriving (Eq, Show, Read)

instance Default MidiIO where
  def = MidiIO def def def False False

#if MIN_VERSION_base(4,11,0)
instance Semigroup MidiIO where
  x <> y          = x `mappendMidiIO` y

instance Monoid MidiIO where
    mempty  = def

#else

instance Monoid MidiIO where
    mempty  = def
    mappend = mappendMidiIO

#endif

mappendMidiIO :: MidiIO -> MidiIO -> MidiIO
mappendMidiIO a b =
  MidiIO
    { midiFile = midiFile a <|> midiFile b
    , midiOutFile = midiOutFile a <|> midiOutFile b
    , muteTracks = muteTracks a <|> muteTracks b
    , rawControllerMode = mappendBool (rawControllerMode a) (rawControllerMode b)
    , terminateOnMidi = mappendBool (terminateOnMidi a) (terminateOnMidi b)
    }

-- MIDI Realtime Input/Ouput

data MidiRT = MidiRT
  { midiDevice :: Maybe Text
  , midiKey :: Maybe Int
  , midiKeyCps :: Maybe Int
  , midiKeyOct :: Maybe Int
  , midiKeyPch :: Maybe Int
  , midiVelocity :: Maybe Int
  , midiVelocityAmp :: Maybe Int
  , midiOutDevice :: Maybe Text
  }
  deriving (Eq, Show, Read)

instance Default MidiRT where
  def =
    MidiRT
      def
      def
      def
      def
      def
      def
      def
      def

#if MIN_VERSION_base(4,11,0)
instance Semigroup MidiRT where
  x <> y          = x `mappendMidiRT` y

instance Monoid MidiRT where
    mempty  = def

#else

instance Monoid MidiRT where
    mempty  = def
    mappend = mappendMidiRT

#endif

mappendMidiRT :: MidiRT -> MidiRT -> MidiRT
mappendMidiRT a b =
  MidiRT
    { midiDevice = midiDevice a <|> midiDevice b
    , midiKey = midiKey a <|> midiKey b
    , midiKeyCps = midiKeyCps a <|> midiKeyCps b
    , midiKeyOct = midiKeyOct a <|> midiKeyOct b
    , midiKeyPch = midiKeyPch a <|> midiKeyPch b
    , midiVelocity = midiVelocity a <|> midiVelocity b
    , midiVelocityAmp = midiVelocityAmp a <|> midiVelocityAmp b
    , midiOutDevice = midiOutDevice a <|> midiOutDevice b
    }

data Rtmidi = PortMidi | AlsaMidi | AlsaSeq | CoreMidi | MmeMidi | WinmmeMidi | VirtualMidi | NoRtmidi
  deriving (Eq, Show, Read)

-- Display

data Displays = Displays
  { csdLineNums :: Maybe Int
  , displayMode :: Maybe DisplayMode
  , displayHeartbeat :: Maybe Int
  , messageLevel :: Maybe Int
  , mAmps :: Maybe Int
  , mRange :: Maybe Int
  , mWarnings :: Maybe Int
  , mDb :: Maybe Int
  , mColours :: Maybe Int
  , mBenchmarks :: Maybe Int
  , msgColor :: Bool
  , displayVerbose :: Bool
  , listOpcodes :: Maybe Int
  }
  deriving (Eq, Show, Read)

data DisplayMode = NoDisplay | PostScriptDisplay | AsciiDisplay
  deriving (Eq, Show, Read)

instance Default Displays where
  def =
    Displays
      def
      (Just NoDisplay)
      def
      def
      def
      def
      def
      def
      def
      def
      False
      False
      def

#if MIN_VERSION_base(4,11,0)
instance Semigroup Displays where
  x <> y          = x `mappendDisplays` y

instance Monoid Displays where
    mempty  = def

#else

instance Monoid Displays where
    mempty  = def
    mappend = mappendDisplays

#endif

mappendDisplays :: Displays -> Displays -> Displays
mappendDisplays a b =
  Displays
    { csdLineNums = csdLineNums a <|> csdLineNums b
    , displayMode = displayMode a <|> displayMode b
    , displayHeartbeat = displayHeartbeat a <|> displayHeartbeat b
    , messageLevel = messageLevel a <|> messageLevel b
    , mAmps = mAmps a <|> mAmps b
    , mRange = mRange a <|> mRange b
    , mWarnings = mWarnings a <|> mWarnings b
    , mDb = mDb a <|> mDb b
    , mColours = mColours a <|> mColours b
    , mBenchmarks = mBenchmarks a <|> mBenchmarks b
    , msgColor = mappendBool (msgColor a) (msgColor b)
    , displayVerbose = mappendBool (displayVerbose a) (displayVerbose b)
    , listOpcodes = listOpcodes a <|> listOpcodes b
    }

-- Performance Configuration and Control

data Config = Config
  { hwBuf :: Maybe Int
  , ioBuf :: Maybe Int
  , newKr :: Maybe Int
  , newSr :: Maybe Int
  , scoreIn :: Maybe Text
  , omacro :: Maybe (Text, Text)
  , smacro :: Maybe (Text, Text)
  , setSched :: Bool
  , schedNum :: Maybe Int
  , strsetN :: Maybe (Int, Text)
  , skipSeconds :: Maybe Double
  , setTempo :: Maybe Int
  }
  deriving (Eq, Show, Read)

instance Default Config where
  def =
    Config
      def
      def
      def
      def
      def
      def
      def
      False
      def
      def
      def
      def

#if MIN_VERSION_base(4,11,0)
instance Semigroup Config where
  x <> y          = x `mappendConfig` y

instance Monoid Config where
    mempty  = def

#else

instance Monoid Config where
    mempty  = def
    mappend = mappendConfig

#endif

mappendConfig :: Config -> Config -> Config
mappendConfig a b =
  Config
    { hwBuf = hwBuf a <|> hwBuf b
    , ioBuf = ioBuf a <|> ioBuf b
    , newKr = newKr a <|> newKr b
    , newSr = newSr a <|> newSr b
    , scoreIn = scoreIn a <|> scoreIn b
    , omacro = omacro a <|> omacro b
    , smacro = smacro a <|> smacro b
    , setSched = mappendBool (setSched a) (setSched b)
    , schedNum = schedNum a <|> schedNum b
    , strsetN = strsetN a <|> strsetN b
    , skipSeconds = skipSeconds a <|> skipSeconds b
    , setTempo = setTempo a <|> setTempo b
    }

----------------------------------------------------
-- rendering

-- just an alias for 'pretty'
p :: (Pretty b) => (a -> Maybe b) -> (a -> Maybe Doc)
p = (fmap pretty .)

pe :: (Pretty b) => (a -> b) -> (a -> Maybe Doc)
pe f = phi . f
  where
    phi x
      | null (show res) = Nothing
      | otherwise = Just res
      where
        res = pretty x

bo :: Text -> (a -> Bool) -> (a -> Maybe Doc)
bo property extract a
  | extract a = Just $ textStrict property
  | otherwise = Nothing

mapPretty :: (Pretty b) => (Doc -> Doc) -> (a -> Maybe b) -> (a -> Maybe Doc)
mapPretty f a = fmap (f . pretty) . a

p1 :: Doc -> Doc -> Doc
p1 pref x = hcat [char '-', pref, char ' ', x]

p2 :: Doc -> Doc -> Doc
p2 pref x = hcat [char '-', char '-', pref, char '=', x]

p3 :: Doc -> Doc -> Doc
p3 pref x = hcat [char '-', char '+', pref, char '=', x]

fields :: [a -> Maybe Doc] -> a -> Doc
fields fs a = hsep $ mapMaybe ($ a) fs

instance Pretty Flags where
  pretty =
    fields
      [ pe displays
      , pe config
      , pe audioFileOutput
      , pe idTags
      , p rtaudio
      , p pulseAudio
      , p rtmidi
      , pe midiIO
      , pe midiRT
      , p flagsVerbatim
      ]

instance Pretty AudioFileOutput where
  pretty =
    fields
      [ pSamplesAndType . (\x -> (formatSamples x, formatType x))
      , mapPretty (p2 "output") output
      , mapPretty (p2 "input") input
      , bo "--nosound" nosound
      , bo "--nopeaks" nopeaks
      , mapPretty (p2 "d/Mither") $ fmap (firstToLower . Text.pack . show) . dither
      ]

pSamplesAndType :: (Maybe FormatSamples, Maybe FormatType) -> Maybe Doc
pSamplesAndType (ma, mb) = fmap pretty $ case (ma, mb) of
  (Nothing, Nothing) -> Nothing
  (Just a, Nothing) -> Just $ p2 "format" $ samplesToStr a
  (Nothing, Just b) -> Just $ p2 "format" $ typeToStr b
  (Just a, Just b) -> Just $ p2 "format" $ samplesAndTypeToStr a b
  where
    samplesToStr x = case x of
      Bit24 -> "24bit"
      FloatSamples -> "float"
      _ -> textStrict $ firstToLower $ Text.pack $ show x

    typeToStr = textStrict . firstToLower . Text.pack . show

    samplesAndTypeToStr a b = hcat [samplesToStr a, ":", typeToStr b]

instance Pretty Dither where
  pretty = p2 "dither" . textStrict . Text.pack . show

instance Pretty IdTags where
  pretty =
    fields
      [ mapPretty (p3 "id_artist") (subst idArtist)
      , mapPretty (p3 "id_comment") (subst idComment)
      , mapPretty (p3 "id_copyright") (subst idCopyright)
      , mapPretty (p3 "id_date") (subst idDate)
      , mapPretty (p3 "id_software") (subst idSoftware)
      , mapPretty (p3 "id_title") (subst idTitle)
      ]
    where
      subst f = fmap (Text.map substSpaces) . f

      substSpaces x
        | isSpace x = '_'
        | otherwise = x

instance Pretty Rtaudio where
  pretty x = case x of
    PortAudio -> rt "PortAudio"
    Jack name ins outs -> rt "jack" <+> jackFields name ins outs
    Mme -> rt "mme"
    Alsa -> rt "alsa"
    CoreAudio -> rt "auhal"
    NoRtaudio -> rt "0"
    where
      rt = p3 "rtaudio"

      jackFields name ins outs =
        hsep
          [ p3 "jack_client" (textStrict name)
          , p3 "jack_inportname" (textStrict ins)
          , p3 "jack_outportname" (textStrict outs)
          ]

instance Pretty PulseAudio where
  pretty a =
    hsep
      [ p3 "server" $ textStrict (paServer a)
      , p3 "output_stream" $ textStrict (paOutput a)
      , p3 "input_stream" $ textStrict (paInput a)
      ]

instance Pretty MidiIO where
  pretty =
    fields
      [ mapPretty (p2 "midifile") midiFile
      , mapPretty (p2 "midioutfile") midiOutFile
      , mapPretty (p3 "mute_tracks") muteTracks
      , bo "-+raw_controller_mode" rawControllerMode
      , bo "--terminate-on-midi" terminateOnMidi
      ]

instance Pretty MidiRT where
  pretty =
    fields
      [ mapPretty (p2 "midi-device") midiDevice
      , mapPretty (p2 "midi-key") midiKey
      , mapPretty (p2 "midi-key-cps") midiKeyCps
      , mapPretty (p2 "midi-key-oct") midiKeyOct
      , mapPretty (p2 "midi-key-pch") midiKeyPch
      , mapPretty (p2 "midi-velocity") midiVelocity
      , mapPretty (p2 "midi-velocity-amp") midiVelocityAmp
      , mapPretty (p1 "Q") midiOutDevice
      ]

instance Pretty Rtmidi where
  pretty x = p3 "rtmidi" $ case x of
    VirtualMidi -> "virtual"
    PortMidi -> "PortMidi"
    AlsaMidi -> "alsa"
    AlsaSeq -> "alsaseq"
    CoreMidi -> "coremidi"
    MmeMidi -> "mme"
    WinmmeMidi -> "winmme"
    NoRtmidi -> "0"

instance Pretty Displays where
  pretty =
    fields
      [ mapPretty (p2 "csd-line-nums") csdLineNums
      , p displayMode
      , mapPretty (p2 "heartbeat") displayHeartbeat
      , mapPretty (p2 "messagelevel") messageLevel
      , mapPretty (p2 "m-amps") mAmps
      , mapPretty (p2 "m-range") mRange
      , mapPretty (p2 "m-warnings") mWarnings
      , mapPretty (p2 "m-dB") mDb
      , mapPretty (p2 "m-colours") mColours
      , mapPretty (p2 "m-benchmarks") mBenchmarks
      , bo "-+msg_color" msgColor
      , bo "--verbose" displayVerbose
      , mapPretty (p2 "list-opcodes") listOpcodes
      ]

instance Pretty DisplayMode where
  pretty x = case x of
    NoDisplay -> "--nodisplays"
    PostScriptDisplay -> "--postscriptdisplay"
    AsciiDisplay -> "--asciidisplay"

instance Pretty Config where
  pretty =
    fields
      [ mapPretty (p2 "hardwarebufsamps") hwBuf
      , mapPretty (p2 "iobufsamps") ioBuf
      , mapPretty (p2 "control-rate") newKr
      , mapPretty (p2 "sample-rate") newSr
      , mapPretty (p2 "score-in") scoreIn
      , macro "omacro" omacro
      , macro "smacro" smacro
      , bo "--sched" setSched
      , mapPretty (p2 "sched") schedNum
      , strset strsetN
      , mapPretty (p3 "skip_seconds") skipSeconds
      , mapPretty (p2 "tempo") setTempo
      ]
    where
      macro :: Doc -> (a -> Maybe (Text, Text)) -> a -> Maybe Doc
      macro name f = fmap phi . f
        where
          phi (a, b) = hcat ["--", pretty name, ":", textStrict a, "=", textStrict b]

      strset f = fmap phi . f
        where
          phi (n, a) = hcat ["--strset", int n, "=", textStrict a]

---------------------------------------------------
-- utilities

firstToLower :: Text -> Text
firstToLower x = case Text.uncons x of
  Just (a, as) -> Text.cons (toLower a) as
  Nothing -> x
