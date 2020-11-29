module Csound.Options(
    Options(..),

    -- * Shortcuts
    setDur,
    setRates, setBufs, setGain,
    setJack, setJackConnect, setAlsa, setCoreAudio, setMme,
    setOutput, setInput,
    setDac, setAdc, setDacBy, setAdcBy, setThru,
    setSilent, setMidiDevice, setMa,
    setMessageLevel, noMessages, setTrace,
    setCabbage,
    setJacko,

    -- * Flags
    -- | Csound's command line flags. See original documentation for
    -- detailed overview: <http://www.csounds.com/manual/html/CommandFlagsCategory.html>
    Flags(..),

    -- * Audio file output
    AudioFileOutput(..),
    FormatHeader(..), FormatSamples(..), FormatType(..),
    Dither(..), IdTags(..),

    -- * Realtime Audio Input/Output
    Rtaudio(..), PulseAudio(..),

    -- * MIDI File Input/Ouput
    MidiIO(..),

    -- * MIDI Realtime Input/Ouput
    MidiRT(..), Rtmidi(..),

    -- * Display
    Displays(..), DisplayMode(..),

    -- * Performance Configuration and Control
    Config(..)
) where

import Data.Default
import Csound.Typed

-- | Sets sample rate and block size
--
-- > setRates sampleRate blockSize
setRates :: Int -> Int -> Options
setRates sampleRate blockSize = def
    { csdSampleRate = Just sampleRate
    , csdBlockSize  = Just blockSize }

-- | Sets hardware and software buffers.
--
-- > setBufs hardwareBuf ioBuf
setBufs :: Int -> Int -> Options
setBufs hw io = def { csdFlags = def { config = def { hwBuf = Just hw, ioBuf = Just io } } }

-- | Sets the default gain for the output signal (should be in range 0 to 1).
setGain :: Double -> Options
setGain d = def { csdGain = Just d' }
    where d' = max 0 $ min 1 $ d

-- | Runs as JACK unit with given name (first argument).
setJack :: String -> Options
setJack name = def { csdFlags = def { rtaudio = Just $ Jack name "input" "output" } }

-- | Defines a header for a Jacko opcodes. The Jacko opcodes allow for greater flexibility
-- with definition of Jack-client. See the Csound docs for details and the datatype @Jacko@.
--
-- > csound doc: <http://csound.github.io/docs/manual/JackoOpcodes.html>
setJacko :: Jacko -> Options
setJacko jackoSpec = def { csdJacko = Just jackoSpec }

-- | Sets real-time driver to Core Audio (use on OSX).
setCoreAudio :: Options
setCoreAudio = def { csdFlags = def { rtaudio = Just $ CoreAudio } }

-- | Sets real-time driver to Alsa (use on Linux).
setAlsa :: Options
setAlsa = def { csdFlags = def { rtaudio = Just $ Alsa } }

-- | Sets real-time driver to Mme (use on Windows).
setMme :: Options
setMme = def { csdFlags = def { rtaudio = Just $ Mme } }

-- | Sends output to speakers.
setDac :: Options
setDac = setDacBy ""

-- | Reads input from audio-card inputs.
setAdc :: Options
setAdc = setAdcBy ""

-- | Set's the input name of the device or file.
setInput :: String -> Options
setInput a = def { csdFlags = def { audioFileOutput = def { input = Just a } } }

-- | Set's the output name of the device or file.
setOutput :: String -> Options
setOutput a = def { csdFlags = def { audioFileOutput = def { output = Just a } } }

-- | Provides name identifier for dac.
setDacBy :: String -> Options
setDacBy port = setOutput name
    where name
            | null port = "dac"
            | otherwise = "dac:" ++ port

-- | Provides name identifier for adc.
setAdcBy :: String -> Options
setAdcBy port = setInput name
    where name
            | null port = "adc"
            | otherwise = "adc:" ++ port

-- | Sets both dac and adc.
setThru :: Options
setThru = mappend setDac setAdc

-- | Sets the output to nosound.
setSilent :: Options
setSilent = (def { csdFlags = def { audioFileOutput = def { nosound = True } } })

-- | Sets midi device. It's an string identifier of the device.
--
-- Read MIDI events from device DEVICE. If using ALSA MIDI (-+rtmidi=alsa),
-- devices are selected by name and not number. So, you need to use an option
-- like -M hw:CARD,DEVICE where CARD and DEVICE are the card and device numbers (e.g. -M hw:1,0).
-- In the case of PortMidi and MME, DEVICE should be a number, and if it is out of range,
-- an error occurs and the valid device numbers are printed. When using PortMidi,
-- you can use '-Ma' to enable all devices. This is also convenient when you
-- don't have devices as it will not generate an error.
setMidiDevice :: String -> Options
setMidiDevice a = def { csdFlags = def { midiRT = def { midiDevice = Just a } } }

-- | Sets midi device to all.
setMa :: Options
setMa = setMidiDevice "a"

-- | Sets message level. For input integer value consult
-- the Csound docs
--
-- <http://csound.com/docs/manual/CommandFlagsCategory.html>
setMessageLevel :: Int -> Options
setMessageLevel n = def { csdFlags = def { displays = def { messageLevel = Just n }}}

-- | Sets the tracing or debug info of csound console to minimum.
noMessages :: Options
noMessages = setMessageLevel 0

setTrace :: Options
setTrace = def { csdTrace = Just True }

---------------------------------------------

-- | Provides options for Cabbage VST-engine.
setCabbage :: Options
setCabbage = setRates 48000 64 <> setNoRtMidi <> setMidiDevice "0"
    where setNoRtMidi = def { csdFlags = def { rtmidi = Just NoRtmidi, audioFileOutput = def { nosound = True } }}

-- | Defines what ports we should connect after application is launched
--
-- It invokes @jack_connect@ for every pair of port-names in the list.
setJackConnect :: [(String, String)] -> Options
setJackConnect connections = def { csdJackConnect = Just connections }
