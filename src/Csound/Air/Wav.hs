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
    ramTab, mincer, temposcal,
    Phsr(..), lphase, relPhsr, sndPhsr, phsrBounce, phsrOnce,
    ram, ram1,

    -- ** Simple audio reading functions (Stereo)
    Fidelity, TempoSig, PitchSig,

    readRam, loopRam, readSeg, loopSeg, readRel, loopRel,

    -- ** Simple audio reading functions (Mono)

    readRam1, loopRam1, readSeg1, loopSeg1, readRel1, loopRel1,

    -- ** Scaling audio files
    scaleDrum, scaleHarm, scaleDrum1, scaleHarm1, scaleWav1, scaleWav,

    -- * Writing sound files
    SampleFormat(..),
    writeSigs, writeWav, writeAiff, writeWav1, writeAiff1,
    dumpWav, dumpWav1,

    -- * Utility
    lengthSnd, segments,

    -- * Signal manipulation
    takeSnd, delaySnd, afterSnd, lineSnd, loopLineSnd, segmentSnd, repeatSnd, toMono
) where

import Data.List(isSuffixOf)
import Data.Default
import Data.Boolean
import Control.Applicative hiding((<*))

import Temporal.Media

import Control.Monad.Trans.Class
import Csound.Dynamic hiding (int, Sco)

import Csound.Typed
import Csound.Typed.Opcode
import Csound.Tab(mp3s, mp3Left, wavs, wavLeft, WavChn(..), Mp3Chn(..))
import Csound.Control.Instr(withDur, sched)

import Csound.SigSpace(mapSig)
import Csound.Control.Evt(metroE, loadbang)

import Csound.Air.Spec

--------------------------------------------------------------------------
-- Signal manipulation

-- | Takes only given amount (in seconds) from the signal (the rest is silence).
takeSnd :: Sigs a => D -> a -> a
takeSnd dt asig = sched (const $ return asig) $ withDur dt $ loadbang

-- | Delays signals by the given amount (in seconds).
delaySnd :: Sigs a => D -> a -> a
delaySnd dt = segmentSnd dt infiniteDur 
    
-- | Delays a signal by the first argument and takes only second argument amount
-- of signal (everything is measured in seconds).
segmentSnd ::Sigs a => D -> D -> a -> a
segmentSnd dt dur asig = sched (const $ return asig) $ fmap (del dt) $ withDur dur $ loadbang 

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
segments :: D -> Evt (Sco Unit)
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

-- | Dumps signals to file and sends the audio through. Useful to monitor the signals.
dumpWav :: String -> (Sig, Sig) -> SE (Sig, Sig)
dumpWav file asig = writeWav file asig >> return asig

-- | Dumps mono signal to file and sends the audio through. Useful to monitor the signals.
dumpWav1 :: String -> Sig -> SE Sig
dumpWav1 file asig = writeWav file (asig, asig) >> return asig

-- | Writes aiff files.
writeAiff :: String -> (Sig, Sig) -> SE ()
writeAiff file = writeSigs Aiff Int16 file . \(a, b) -> [a, b]

-- | Writes mono signals to wav files.
writeWav1 :: String -> Sig -> SE ()
writeWav1 file = writeWav file . \x -> (x, x)

-- | Writes mono signals to aiff files.
writeAiff1 :: String -> Sig -> SE ()
writeAiff1 file = writeAiff file . \x -> (x, x)

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-- mincer

-- | mincer — Phase-locked vocoder processing.
--
-- mincer implements phase-locked vocoder processing 
-- using function tables containing sampled-sound sources, 
-- with GEN01, and mincer will accept deferred allocation tables.
--
-- This opcode allows for time and frequency-independent scaling. 
-- Time is controlled by a time index (in seconds) to the function 
-- table position and can be moved forward and backward at any 
-- chosen speed, as well as stopped at a given position ("frozen"). 
-- The quality of the effect is generally improved with phase locking switched on.
--
-- > asig mincer atimpt, kamp, kpitch, ktab, klock[,ifftsize,idecim]
--
-- csound doc: <http://www.csounds.com/manual/html/mincer.html>
mincer ::  Sig -> Sig -> Sig -> Tab -> Sig -> Sig
mincer b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4 <*> unSig b5    
    where f a1 a2 a3 a4 a5 = opcs "mincer" [(Ar,[Ar,Kr,Kr,Kr,Kr,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | temposcal — Phase-locked vocoder processing with onset detection/processing, 'tempo-scaling'. 
--
-- temposcal implements phase-locked vocoder processing using function tables containing 
-- sampled-sound sources, with GEN01, and temposcal will accept deferred allocation tables.
--
-- This opcode allows for time and frequency-independent scaling. Time is advanced internally, 
-- but controlled by a tempo scaling parameter; when an onset is detected, timescaling is 
-- momentarily stopped to avoid smearing of attacks. The quality of the effect is generally 
-- improved with phase locking switched on.
--
-- temposcal will also scale pitch, independently of frequency, using a transposition factor (k-rate). 
--
-- > asig temposcal ktimescal, kamp, kpitch, ktab, klock [,ifftsize, idecim, ithresh]
--
-- csound doc: <http://www.csounds.com/manual/html/temposcal.html>
temposcal :: Sig -> Sig -> Sig -> Tab -> Sig -> Sig
temposcal b1 b2 b3 b4 b5 = Sig $ f <$> unSig b1 <*> unSig b2 <*> unSig b3 <*> unTab b4 <*> unSig b5    
    where f a1 a2 a3 a4 a5 = opcs "temposcal" [(Ar,[Kr,Kr,Kr,Kr,Kr,Ir,Ir,Ir])] [a1,a2,a3,a4,a5]

-- | Mincer. We can playback a table and scale by tempo and pitch.
--
-- > mincer fidelity table pointer pitch 
--
-- fidelity is the parameter that specifies the size of the window (for FFT transform).
-- The size equals to formula (fidelity + 11) ^ 2. If you don't know what to choose
-- choose 0 for pitched sounds and -2 for drums. The table contains the sample to playback.
-- The pointer loops over the table. The pitch specifies a scaling factor for pitch.
-- So we can raise tone an octave up by setting the pitch to 2.
ramTab :: Fidelity -> Tab -> Sig -> Sig -> Sig
ramTab winSizePowerOfTwo tab aptr pitch = mincer aptr 1 pitch tab 1 `withD` (2 ** (winSizePowerOfTwo + 11))


-- > let x n = mincer2 (Phsr "/home/anton/fox.wav" 0 (stepSeq [0.2, 1, 0.1, 0.5] 0.5) (lpshold [1, 0.8, -1, 0.2] 0.25)) n
-- > dac $ mul 3 $ at (lp18 0.7 800 0.1) $ cfd (slide 0.5 $ usqr 0.2) (x 1) (sum [x $ 6/5, x $ 2])

-- | Creates a pointer signal for reading audio from the table in loops.
--
-- > lphase length start end speed
--
-- Arguments are:
--
-- * length of the table  in seconds
--
-- * start and end points of the reading interval
--
-- * playback speed
lphase :: D -> Sig -> Sig -> Sig -> Sig
lphase irefdur kloopstart kloopend kspeed  = atimpt
    where
        kfqrel = kspeed / (kloopend - kloopstart)
        andxrel = phasor kfqrel
        atimpt = andxrel * (kloopend-kloopstart) + kloopstart

----------------------------------------------------------------------

-- | Looping phasor. It creates a looping pointer to the file.
-- It's used in the function ram.
-- 
-- Ther arguments are: file name, start and end of the looping segment (in seconds),
-- and the playback speed.
data Phsr = Phsr
    { phsrFile  :: String
    , phsrStart :: Sig
    , phsrEnd   :: Sig
    , phsrSpeed :: Sig
    }

-- | Forces phasor to play only once.
phsrOnce :: Phsr -> Phsr 
phsrOnce a = a { phsrSpeed = phsrSpeed a * linseg [1, dt, 1, 0.01, 0] }
    where dt = ir $ abs $ (phsrEnd a - phsrStart a) / phsrSpeed a

-- | Reads the file forth and back. 
phsrBounce :: Phsr -> Phsr
phsrBounce a = a { phsrSpeed = phsrSpeed a * sqr (1 / dt) }
    where dt = abs $ (phsrEnd a - phsrStart a) / phsrSpeed a

-- | Creates a phasor if segments are relative to the total length.
-- It can be useful for drum loops. If we don't know the complete length
-- but we know that loop contains four distinct parts.
relPhsr :: String -> Sig -> Sig -> Sig -> Phsr
relPhsr file start end speed = Phsr
    { phsrFile  = file
    , phsrStart = start * sig len
    , phsrEnd   = end   * sig len
    , phsrSpeed = speed }
    where len = filelen $ text file

-- | Creates a phasor for reading the whole audio file  in loops 
-- with given speed.
sndPhsr :: String -> Sig -> Phsr
sndPhsr file speed = relPhsr file 0 1 speed

ram1 :: Fidelity -> Phsr -> Sig -> Sig
ram1 = ramChn True 1

-- | Reads audio files in loops. The file is loaded in RAM. 
-- The size of the file is limited. It should be not more than 6 minutes
-- for sample rate of 44100. 5.9 minutes for 48000.
--
-- What makes this function so cool is
-- that we can scale the sound by tempo
-- without affecting pitch, and we can scale the sound by pitch
-- without affecting the tempo. Let's study the arguments.
--
-- > ram fidelity phasor pitch 
--
-- fidelity corresponds to the size of the FFT-window. 
-- The function performs the FFT transform and it has to know the size.
-- It's not the value for the size it's an integer value
-- that proportional to the size. The higher the value the higher the size
-- the lower the value the lower the size. The default value is 0. 
-- Zero is best for most of the cases. For drums we can lower it to (-2).
--
-- The phasor is a quadruple of values
--
-- > (Phsr fileName startTime endTime playbackSpeed)
--
-- we can read the file from startTime to endTime (in seconds)
-- and we can set the speed for playback. If speed is negative
-- file is played in reverse. The playback is looped.
-- So to scale the tempo or play in reverse we can change the playbackSpeed.
-- 
-- The last argument is pitch factor. We can rise by octave with factor 2.
-- It's good place to use the function semitone. It produces factors for a number in semitones.
-- 
-- Note that all parameters (except window size) are signals.
-- It makes this function very flexible. We can change the speed of playback
-- and start and end of the reading segment as we wish.
--
-- > ram 0 (Phsr "file.wav" 0 1 1.2) 1
--
-- PS: here is the formula for window size: 2 ** (fidelity + 11)
ram :: Fidelity -> Phsr -> Sig -> Sig2
ram winSize phsr pitch = (ramChn False 1 winSize phsr pitch, ramChn False 2 winSize phsr pitch)
    
ramChn :: Bool -> Int -> Fidelity -> Phsr -> Sig -> Sig
ramChn isMono n winSize (Phsr file start end speed) pitch = 
    ifB (abs speed `lessThan` 0.001) 0 $ 
        ramTab winSize (mkTab isMono n file ) (lphase (filelen $ text file) start end (speed * srFactor)) (pitch * srFactor)
    where srFactor = sig $ (filesr $ text file) / getSampleRate

mkTab :: Bool -> Int ->  String -> Tab
mkTab isMono chn file 
    | mp3 && isMono    = mp3s file 0 Mp3Mono
    | mp3 && isStereo  = mp3s file 0 (if chn == 1 then Mp3Left else Mp3Right)
    | otherwise        = wavs file 0 (if chn == 1 then WavLeft else WavRight)
    where 
        mp3 = isMp3 file        
        isStereo = not isMono

----------------------------------------
-- std funs

-- | Fidelity corresponds to the size of the FFT-window that is used by functions of RAM-family. 
-- The function performs the FFT transform and it has to know the size.
-- It's not the value for the size it's an integer value
-- that proportional to the size. The higher the value the higher the size
-- the lower the value the lower the size. The default value is 0. 
-- Zero is best for most of the cases. For drums we can lower it to (-2).
--
-- PS: here is the formula for window size: 2 ** (fidelity + 11).
-- So the fidelity is actually the degree for power of two. 
-- The FFT-algorithm requires the window size to be a power of two.
--
-- The lower fidelity is the less power is consumed by the function.
type Fidelity = D

-- | Scaling factor for tempo. The 1 is inherent tempo.
type TempoSig = Sig

-- | Scaling factor for pitch. The 1 is inherent pitch.
type PitchSig = Sig

-- | Reads file once and scales it by tempo and pitch.
readRam :: Fidelity -> TempoSig-> PitchSig -> String -> Sig2
readRam winSize tempo pitch file = ram winSize (phsrOnce $ sndPhsr file tempo) pitch

-- | Loop over file and scales it by tempo and pitch (it's based on mincer opcode).
loopRam :: Fidelity -> TempoSig-> PitchSig -> String -> Sig2
loopRam winSize tempo pitch file = ram winSize (sndPhsr file tempo) pitch

-- | Reads a segment from file once and scales it by tempo and pitch.
-- Segment is defined in seconds.
readSeg :: Fidelity -> (Sig, Sig) -> TempoSig-> PitchSig -> String -> Sig2
readSeg winSize (kmin, kmax) tempo pitch file = ram winSize (phsrOnce $ Phsr file kmin kmax tempo) pitch

-- | Loops over a segment of file and scales it by tempo and pitch.
-- Segment is defined in seconds.
loopSeg :: Fidelity -> (Sig, Sig) -> TempoSig-> PitchSig -> String -> Sig2
loopSeg winSize (kmin, kmax) tempo pitch file = ram winSize (Phsr file kmin kmax tempo) pitch

-- | Reads a relative segment from file once and scales it by tempo and pitch.
-- Segment is defined in seconds. The end ponits for the segment are relative to the
-- total length of the file.
readRel :: Fidelity -> (Sig, Sig) -> TempoSig-> PitchSig -> String -> Sig2
readRel winSize (kmin, kmax) tempo pitch file = ram winSize (phsrOnce $ relPhsr file kmin kmax tempo) pitch

-- | Loops over a relative segment of file and scales it by tempo and pitch.
-- Segment is defined in seconds. The end ponits for the segment are relative to the
-- total length of the file.
loopRel :: Fidelity -> (Sig, Sig) -> TempoSig-> PitchSig -> String -> Sig2
loopRel winSize (kmin, kmax) tempo pitch file = ram winSize (relPhsr file kmin kmax tempo) pitch

-- | The mono version of readRam.
readRam1 :: Fidelity -> TempoSig-> PitchSig -> String -> Sig
readRam1 winSize tempo pitch file = ram1 winSize (phsrOnce $ sndPhsr file tempo) pitch

-- | The mono version of loopRam.
loopRam1 :: Fidelity -> TempoSig-> PitchSig -> String -> Sig
loopRam1 winSize tempo pitch file = ram1 winSize (sndPhsr file tempo) pitch

-- | The mono version of readSeg.
readSeg1 :: Fidelity -> (Sig, Sig) -> TempoSig-> PitchSig -> String -> Sig
readSeg1 winSize (kmin, kmax) tempo pitch file = ram1 winSize (phsrOnce $ Phsr file kmin kmax tempo) pitch

-- | The mono version of loopSeg.
loopSeg1 :: Fidelity -> (Sig, Sig) -> TempoSig-> PitchSig -> String -> Sig
loopSeg1 winSize (kmin, kmax) tempo pitch file = ram1 winSize (Phsr file kmin kmax tempo) pitch

-- |  The mono version of readRel.
readRel1 :: Fidelity -> (Sig, Sig) -> TempoSig-> PitchSig -> String -> Sig
readRel1 winSize (kmin, kmax) tempo pitch file = ram1 winSize (phsrOnce $ relPhsr file kmin kmax tempo) pitch

-- |  The mono version of loopRel.
loopRel1 :: Fidelity -> (Sig, Sig) -> TempoSig-> PitchSig -> String -> Sig
loopRel1 winSize (kmin, kmax) tempo pitch file = ram1 winSize (relPhsr file kmin kmax tempo) pitch

------------------------------------
-- scaling tempo/pitch based on temposcale

-- | ScaleWav function with fidelity set for drum-loops.
scaleDrum :: TempoSig -> PitchSig -> String -> Sig2
scaleDrum = scaleWav (-2)

-- | ScaleWav function with fidelity set for hormonical-loops.
scaleHarm :: TempoSig -> PitchSig -> String -> Sig2
scaleHarm = scaleWav 0

-- | ScaleWav1 function with fidelity set for drum-loops.
scaleDrum1 :: TempoSig -> PitchSig -> String -> Sig
scaleDrum1 = scaleWav1 (-2)

-- | ScaleWav1 function with fidelity set for hormonical-loops.
scaleHarm1 :: TempoSig -> PitchSig -> String -> Sig
scaleHarm1 = scaleWav1 0

-- | Scaling mono audio files (accepts both midi and wav). It's based on temposcal Csound opcode.
scaleWav1 :: Fidelity -> TempoSig -> PitchSig -> String -> Sig
scaleWav1 winSizePowerOfTwo tempo pitch filename = go $ if mp3 then mp3Left filename else wavLeft filename
    where 
        go = simpleTempoScale winSizePowerOfTwo tempo pitch            
        mp3 = isMp3 filename        


-- | Scaling stereo audio files (accepts both midi and wav). It's based on temposcal Csound opcode.
scaleWav :: Fidelity -> TempoSig -> PitchSig -> String -> Sig2
scaleWav winSizePowerOfTwo tempo pitch filename = (go $ mkTab False 0 filename, go $ mkTab False 1 filename) 
    where go = simpleTempoScale winSizePowerOfTwo tempo pitch

simpleTempoScale winSizePowerOfTwo tempo pitch t = temposcal tempo 1 pitch t 1 `withD` (2 ** (winSizePowerOfTwo + 11))
