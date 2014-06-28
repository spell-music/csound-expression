module Csound.Options(
    Options(..),

    -- * Shortcuts
    setDur,
    setRates, setBufs, setGain, setJack,
    setOutput, setInput, 
    setDac, setAdc, setDacBy, setAdcBy, setThru,

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

