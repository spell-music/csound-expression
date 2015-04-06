 -- | Sound file playback
module Csound.Air.Wav(
    -- * Stereo
    readSnd, loopSnd, loopSndBy, 
    readWav, loopWav, readSegWav, 
    tempoLoopWav, tempoReadWav,
    
    -- * Mono
    readSnd1, loopSnd1, loopSndBy1, 
    readWav1, loopWav1, readSegWav1,
    tempoLoopWav1, tempoReadWav1,
    
    -- * Read sound with RAM
    -- 
    -- Loads the sample in the table and plays it back from RAM. The sample should be short. The size of the table is limited.
    -- It's up to 6 minutes for 44100 sample rate, 5 minutes for 48000 and 2.8 minutes for 96000.
    LoopMode(..), ramSnd, ramSnd1, 

    -- * Writing sound files
    SampleFormat(..),
    writeSigs, writeWav, writeAiff, writeWav1, writeAiff1,

    -- * Utility
    lengthSnd, segments,

    -- * Signal manipulation
    takeSnd, delaySnd, afterSnd, lineSnd, loopLineSnd, segmentSnd, repeatSnd, toMono
) where

import Data.List(isSuffixOf)
import Data.Default

import Csound.Typed
import Csound.Typed.Opcode
import Csound.Tab(mp3s, wavs, WavChn(..), Mp3Chn(..))
import Csound.Control.Instr(withDur, sched)

import Csound.SigSpace(mapSig)
import Csound.Control.Evt(metroE, eventList)

import Csound.Air.Spec

--------------------------------------------------------------------------
-- Signal manipulation

-- | Takes only given amount (in seconds) from the signal (the rest is silence).
takeSnd :: Sigs a => D -> a -> a
takeSnd dt asig = trigs (const $ return asig) $ eventList [(0, dt, unit)]

-- | Delays signals by the given amount (in seconds).
delaySnd :: Sigs a => D -> a -> a
delaySnd dt asig = trigs (const $ return asig) $ eventList [(dt, infiniteDur, unit)]

-- | Delays a signal by the first argument and takes only second argument amount
-- of signal (everything is measured in seconds).
segmentSnd ::Sigs a => D -> D -> a -> a
segmentSnd del dur asig = trigs (const $ return asig) $ eventList [(del, dur, unit)]

-- | Repeats the signal with the given period.
repeatSnd :: Sigs a => D -> a -> a
repeatSnd dt asig = sched (const $ return asig) $ segments dt

-- | Plays the first signal for some time (in seconds) and then switches to the next one.
--
-- > afterSnd dur sig1 sig2
afterSnd :: (Num b, Sigs b) => D -> b -> b -> b
afterSnd dt a b = takeSnd dt a + delaySnd dt b

-- | Creates a sequence of signals. Each segment lasts for 
-- fixed amount of time given in the first argument.
lineSnd :: (Num a, Sigs a) => D -> [a] -> a
lineSnd dt xs = foldr1 go xs
    where
        go a b = afterSnd dt a b

-- | Creates a sequence of signals and loops over the sequence. 
-- Each segment lasts for  fixed amount of time given in the first argument.
loopLineSnd :: (Num a, Sigs a) => D -> [a] -> a
loopLineSnd dt xs = repeatSnd (dt * (int $ length xs)) $ lineSnd dt xs

--------------------------------------------------------------------------
-- sound files playback

isMp3 :: String -> Bool
isMp3 name = ".mp3" `isSuffixOf` name

-- | Converts stereosignal to mono with function mean.
toMono :: (Sig, Sig) -> Sig
toMono (a, b) = 0.5 * a + 0.5 * b

-- | Length in seconds of the sound file.
lengthSnd :: String -> D
lengthSnd fileName
    | isMp3 fileName    = mp3len $ text fileName
    | otherwise         = filelen $ text fileName

-- | Produces repeating segments with the given time in seconds.
segments :: D -> Evt (D, Unit)
segments dt = withDur dt $ metroE (sig $ recip dt)

-- Stereo

-- | Reads stereo signal from the sound-file (wav or mp3 or aiff).
readSnd :: String -> (Sig, Sig)
readSnd fileName
    | isMp3 fileName = mp3in (text fileName)        
    | otherwise      = diskin2 (text fileName) 1

-- | Reads stereo signal from the sound-file (wav or mp3 or aiff)
-- and loops it with the given period (in seconds).
loopSndBy :: D -> String -> (Sig, Sig)
loopSndBy dt fileName = repeatSnd dt $ readSnd fileName

-- | Reads stereo signal from the sound-file (wav or mp3 or aiff)
-- and loops it with the file length.
loopSnd :: String -> (Sig, Sig)
loopSnd fileName = loopSndBy (lengthSnd fileName) fileName

-- | Reads the wav file with the given speed (if speed is 1 it's a norma playback).
-- We can use negative speed to read file in reverse.
readWav :: Sig -> String -> (Sig, Sig)
readWav speed fileName = diskin2 (text fileName) speed

-- | Reads th wav file and loops over it.
loopWav :: Sig -> String -> (Sig, Sig)
loopWav speed fileName = flip withDs [0, 1] $ ar2 $ diskin2 (text fileName) speed

-- | Reads a segment from wav file. 
readSegWav :: D -> D -> Sig -> String -> (Sig, Sig)
readSegWav start end speed fileName = takeSnd (end - start) $ diskin2 (text fileName) speed `withDs` [start, 1]

-- | Reads the wav file with the given speed (if speed is 1 it's a norma playback).
-- We can use negative speed to read file in reverse. Scales the tempo with first argument.
tempoReadWav :: Sig -> String -> (Sig, Sig)
tempoReadWav speed fileName = mapSig (scaleSpec (1 / abs speed)) $ diskin2 (text fileName) speed

-- | Reads th wav file and loops over it. Scales the tempo with first argument.
tempoLoopWav :: Sig -> String -> (Sig, Sig)
tempoLoopWav speed fileName = mapSig (scaleSpec (1 / abs speed)) $ flip withDs [0, 1] $ ar2 $ diskin2 (text fileName) speed

-- Mono

-- | The mono variant of the function @readSnd@.
readSnd1 :: String -> Sig
readSnd1 fileName 
    | isMp3 fileName = toMono $ readSnd fileName
    | otherwise      = diskin2 (text fileName) 1

-- | The mono variant of the function @loopSndBy@.
loopSndBy1 :: D -> String -> Sig
loopSndBy1 dt fileName = repeatSnd dt $ readSnd1 fileName

-- | The mono variant of the function @loopSnd@.
loopSnd1 :: String -> Sig
loopSnd1 fileName = loopSndBy1 (lengthSnd fileName) fileName

-- | The mono variant of the function @readWav@.
readWav1 :: Sig -> String -> Sig
readWav1 speed fileName = diskin2 (text fileName) speed

-- | The mono variant of the function @loopWav@.
loopWav1 :: Sig -> String -> Sig
loopWav1 speed fileName = flip withDs [0, 1] $ diskin2 (text fileName) speed

-- | Reads a segment from wav file.
readSegWav1 :: D -> D -> Sig -> String -> Sig
readSegWav1 start end speed fileName = takeSnd (end - start) $ diskin2 (text fileName) speed `withDs` [start, 1]

-- | Reads the mono wav file with the given speed (if speed is 1 it's a norma playback).
-- We can use negative speed to read file in reverse. Scales the tempo with first argument.
tempoReadWav1 :: Sig -> String -> Sig
tempoReadWav1 speed fileName = scaleSpec (1 / abs speed) $ readWav1 speed fileName

-- | Reads th mono wav file and loops over it. Scales the tempo with first argument.
tempoLoopWav1 :: Sig -> String -> Sig
tempoLoopWav1 speed fileName = scaleSpec (1 / abs speed) $ loopWav1 speed fileName

--------------------------------------------------------------------------
-- With RAM

data LoopMode = Once | Loop | Bounce
    deriving (Show, Eq, Enum)

-- | Loads the sample in the table. The sample should be short. The size of the table is limited.
-- It's up to 3 minutes for 44100 sample rate (sr), 2.9 minutes for 48000 sr, 1.4 minutes for 96000 sr.
ramSnd :: LoopMode -> Sig -> String -> Sig2
ramSnd loopMode speed file = loscil3 1 speed t `withDs` [1, int $ fromEnum loopMode]
    where t 
            | isMp3 file = mp3s file 0 def
            | otherwise  = wavs file 0 def

-- | Loads the sample in the table. The sample should be short. The size of the table is limited.
-- It's up to 6 minutes for 44100 sample rate (sr), 5.9 minutes for 48000 sr, 2.8 minutes for 96000 sr.
ramSnd1 :: LoopMode -> Sig -> String -> Sig
ramSnd1 loopMode speed file 
    | isMp3 file = (\(aleft, aright) -> 0.5 * (aleft + aright)) $ loscil3 1 speed (mp3s file 0 def) `withDs` [1, int $ fromEnum loopMode]
    | otherwise  = loscil3 1 speed (wavs file 0 WavLeft) `withDs` [1, int $ fromEnum loopMode]

--------------------------------------------------------------------------
-- writing sound files

-- | The sample format.
data SampleFormat 
    = NoHeaderFloat32       -- ^ 32-bit floating point samples without header
    | NoHeaderInt16         -- ^ 16-bit integers without header
    | HeaderInt16           -- ^ 16-bit integers with a header. The header type depends on the render (-o) format
    | UlawSamples           -- ^  u-law samples with a header
    | Int16                 -- ^ 16-bit integers with a header
    | Int32                 -- ^ 32-bit integers with a header 
    | Float32               -- ^ 32-bit floats with a header
    | Uint8                 -- ^ 8-bit unsigned integers with a header
    | Int24                 -- ^ 24-bit integers with a header
    | Float64               -- ^ 64-bit floats with a header
    deriving (Eq, Ord, Enum)

-- | Writes a sound signal to the file with the given format.
-- It supports only four formats: Wav, Aiff, Raw and Ircam.
writeSigs :: FormatType -> SampleFormat -> String -> [Sig] -> SE ()
writeSigs fmt sample file = fout (text file) formatToInt 
    where 
        formatToInt = int $ formatTypeToInt fmt * 10 + fromEnum sample

        formatTypeToInt :: FormatType -> Int
        formatTypeToInt x = case x of
            Wav   -> 1
            Aiff  -> 2
            Raw   -> 3
            Ircam -> 4
            _     -> error $ "Format " ++ (show x) ++ " is not supported in the writeSnd."

-- | Writes wav files.
writeWav :: String -> (Sig, Sig) -> SE ()
writeWav file = writeSigs Wav Int16 file . \(a, b) -> [a, b]

-- | Writes aiff files.
writeAiff :: String -> (Sig, Sig) -> SE ()
writeAiff file = writeSigs Aiff Int16 file . \(a, b) -> [a, b]

-- | Writes mono signals to wav files.
writeWav1 :: String -> Sig -> SE ()
writeWav1 file = writeWav file . \x -> (x, x)

-- | Writes mono signals to aiff files.
writeAiff1 :: String -> Sig -> SE ()
writeAiff1 file = writeAiff file . \x -> (x, x)

