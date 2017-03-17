module Csound.Options(
    Options(..),

    -- * Shortcuts
    setDur,
    setRates, setBufs, setGain, 
    setJack, setAlsa, setCoreAudio, setMme,
    setOutput, setInput, 
    setDac, setAdc, setDacBy, setAdcBy, setThru,
    setSilent, setMidiDevice, setMa,
    setMessageLevel, noTrace,
    setCabbage,

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

import Data.Monoid
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

setGain :: Double -> Options
setGain d = def { csdGain = Just d' }
    where d' = max 0 $ min 1 $ d

setJack :: String -> Options
setJack name = def { csdFlags = def { rtaudio = Just $ Jack name "input" "output" } }

setCoreAudio :: Options
setCoreAudio = def { csdFlags = def { rtaudio = Just $ CoreAudio } }

setAlsa :: Options
setAlsa = def { csdFlags = def { rtaudio = Just $ Alsa } }

setMme :: Options
setMme = def { csdFlags = def { rtaudio = Just $ Mme } }

setDac :: Options
setDac = setDacBy ""

setAdc :: Options
setAdc = setAdcBy ""

setInput :: String -> Options
setInput a = def { csdFlags = def { audioFileOutput = def { input = Just a } } }

setOutput :: String -> Options
setOutput a = def { csdFlags = def { audioFileOutput = def { output = Just a } } }

setDacBy :: String -> Options
setDacBy port = setOutput name
    where name 
            | null port = "dac"
            | otherwise = "dac:" ++ port
 
setAdcBy :: String -> Options
setAdcBy port = setInput name
    where name 
            | null port = "adc"
            | otherwise = "adc:" ++ port

setThru :: Options
setThru = mappend setDac setAdc

-- | Sets the output to nosound.
setSilent :: Options
setSilent = (def { csdFlags = def { audioFileOutput = def { nosound = True } } })

-- | Sets midi device
setMidiDevice :: String -> Options
setMidiDevice a = def { csdFlags = def { midiRT = def { midiDevice = Just a } } }

-- | Sets midi device to all.
setMa :: Options
setMa = setMidiDevice "a"

setMessageLevel :: Int -> Options
setMessageLevel n = def { csdFlags = def { displays = def { messageLevel = Just n }}}

noTrace :: Options
noTrace = setMessageLevel 0

---------------------------------------------

setCabbage :: Options
setCabbage = setRates 48000 64 <> setNoRtMidi <> setMidiDevice "0"
    where setNoRtMidi = def { csdFlags = def { rtmidi = Just NoRtmidi, audioFileOutput = def { nosound = True } }} 
