{-# Language CPP #-}
module Csound.Core.State.Options (
    Options(..),
    defGain, defSampleRate, defBlockSize, defTabFi, defScaleUI, defNchnls,
    -- * Table fidelity
    TabFi(..), fineFi, coarseFi,
    -- ** Gen identifiers
    -- | Low level Csound integer identifiers for tables. These names can be used in the function 'Csound.Base.fineFi'
    -- *** Integer identifiers
    idWavs, idMp3s, idDoubles, idSines, idSines3, idSines2,
    idPartials, idSines4, idBuzzes, idConsts, idLins, idCubes,
    idExps, idSplines, idStartEnds,  idPolys, idChebs1, idChebs2, idBessels, idWins,
    idTabHarmonics, idMixOnTab, idMixTabs,
    idNormTab, idPolynomFuns, idLinTab, idRandDists, idReadNumFile, idReadNumTab,
    idExpsBreakPoints, idLinsBreakPoints, idReadTrajectoryFile, idMixSines1, idMixSines2,
    idRandHist, idRandPairs, idRandRanges, idPvocex, idTuning, idMultichannel,
    -- *** String identifiers
    idPadsynth, idTanh, idExp, idSone, idFarey, idWave,
    -- * Jacko
    Jacko(..), JackoConnect, renderJacko,
    -- * Debug trace
    csdNeedTrace,

    -- * Shortcuts
    setRates, setBufs, setGain,
    setJack, setJackConnect, setAlsa, setCoreAudio, setMme,
    setOutput, setInput,
    setDac, setAdc, setDacBy, setAdcBy, setThru,
    setSilent, setMidiDevice, setMa,
    setMessageLevel, noMessages, setTrace,
    setJacko,
    setVirtual,
    logTrace,
    -- * Save user options
    saveUserOptions,
    getUserOptions,
) where

import Control.Applicative
import Data.Default
import Data.Maybe

import qualified Data.IntMap as IM
import qualified Data.Map    as M
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Read

import Csound.Dynamic hiding (csdFlags)
import System.Directory
import System.FilePath

-- | Csound options. The default values are
--
-- > flags      = def     -- the only flag set by default is "no-displays"
-- >                      -- to supress the display of the tables
-- > sampleRate = 44100
-- > blockSize  = 64
-- > gain       = 0.5
-- > tabFi      = fineFi 13 [(idLins, 11), (idExps, 11), (idConsts, 9), (idSplines, 11), (idStartEnds, 12)] }
-- > scaleUI    = (1, 1)
data Options = Options
    { csdFlags          :: Flags                    -- ^ Csound command line flags
    , csdSampleRate     :: Maybe Int                -- ^ The sample rate
    , csdBlockSize      :: Maybe Int                -- ^ The number of audio samples in one control step
    , csdNchnlsOut      :: Maybe Int                -- ^ The number of output channels
    , csdNchnlsIn       :: Maybe Int                -- ^ The number of input channels (if Nothing it's not set)
    , csdGain           :: Maybe Double             -- ^ A gain of the final output
    , csdTabFi          :: Maybe TabFi              -- ^ Default fidelity of the arrays
    , csdScaleUI        :: Maybe (Double, Double)   -- ^ Scale factors for UI-window
    , csdJacko          :: Maybe Jacko
    , csdJackConnect    :: Maybe [(Text, Text)] -- ^ list of jack connections to make after csound app is launched (Linux only)
    , csdTrace          :: Maybe Bool               -- ^ Do we need debug-trace, default is False
    , csdRender         :: Maybe RenderOptions
    } deriving (Eq, Show, Read)

instance Default Options where
    def = Options def def def def def def def def def def def def

#if MIN_VERSION_base(4,11,0)
instance Semigroup Options where
    (<>) = mappendOptions

instance Monoid Options where
    mempty  = def

#else

instance Monoid Options where
    mempty  = def
    mappend = mappendOptions

#endif

mappendOptions :: Options -> Options -> Options
mappendOptions a b = Options
    { csdFlags          = mappend (csdFlags a) (csdFlags b)
    , csdSampleRate     = csdSampleRate a <|> csdSampleRate b
    , csdBlockSize      = csdBlockSize a <|> csdBlockSize b
    , csdGain           = csdGain a <|> csdGain b
    , csdNchnlsOut      = csdNchnlsOut a <|> csdNchnlsOut b
    , csdNchnlsIn       = csdNchnlsIn a <|> csdNchnlsIn b
    , csdTabFi          = csdTabFi a <|> csdTabFi b
    , csdScaleUI        = csdScaleUI a <|> csdScaleUI b
    , csdJacko          = csdJacko a <|> csdJacko b
    , csdJackConnect    = mappend (csdJackConnect a) (csdJackConnect b)
    , csdTrace          = csdTrace a <|> csdTrace b
    , csdRender         = csdRender a <|> csdRender b
    }

defScaleUI :: Options -> (Double, Double)
defScaleUI = fromMaybe (1, 1) . csdScaleUI

defGain :: Options -> Double
defGain = fromMaybe 0.8 . csdGain

defSampleRate :: Options -> Int
defSampleRate = fromMaybe 44100 . csdSampleRate

defBlockSize :: Options -> Int
defBlockSize = fromMaybe 64 . csdBlockSize

-- | Stereo audio by default
defNchnls :: Options -> Int
defNchnls = max 1 . fromMaybe 2 . csdNchnlsOut

defTabFi :: Options -> TabFi
defTabFi = fromMaybe def . csdTabFi

---------------------------------------------------------------------------------------------------
-- tables

-- | Table size fidelity (how many points in the table by default).
data TabFi = TabFi
    { tabFiBase   :: Int
    , tabFiGens   :: IM.IntMap Int
    , tabNamedFiGens :: M.Map Text Int
    } deriving (Eq, Show, Read)

instance Default TabFi where
    def = fineFi 13
                [(idLins, 11), (idExps, 11), (idConsts, 9), (idSplines, 11), (idStartEnds, 12), (idExpsBreakPoints, 11), (idLinsBreakPoints, 11), (idRandDists, 6)]
                [(idPadsynth, 18), (idSone, 14), (idTanh, 13), (idExp, 13)]

-- | Sets different table size for different GEN-routines.
--
-- > fineFi n ps
--
-- where
--
-- * @n@ is the default value for table size (size is a @n@ power of 2) for all gen routines that are not listed in the next argument @ps@.
--
-- * @ps@ is a list of pairs @(genRoutineId, tableSizeDegreeOf2)@ that sets the given table size for a
--   given GEN-routine.
--
-- with this function we can set lower table sizes for tables that are usually used in the envelopes.
fineFi :: Int -> [(Int, Int)] -> [(Text, Int)] -> TabFi
fineFi n xs ys = TabFi n (IM.fromList xs) (M.fromList ys)

-- | Sets the same table size for all tables.
--
-- > coarseFi n
--
-- where @n@  is a degree of 2. For example, @n = 10@ sets size to 1024 points for all tables by default.
coarseFi :: Int -> TabFi
coarseFi n = TabFi n IM.empty M.empty

idWavs, idMp3s, idDoubles, idSines, idSines3, idSines2,
    idPartials, idSines4, idBuzzes, idConsts, idLins, idCubes,
    idExps, idSplines, idStartEnds,  idPolys, idChebs1, idChebs2, idBessels, idWins,
    idTabHarmonics, idMixOnTab, idMixTabs,
    idNormTab, idPolynomFuns, idLinTab, idRandDists, idReadNumFile, idReadNumTab,
    idExpsBreakPoints, idLinsBreakPoints, idReadTrajectoryFile, idMixSines1, idMixSines2,
    idRandHist, idRandPairs, idRandRanges, idPvocex, idTuning, idMultichannel :: Int

-- Human readable Csound identifiers for GEN-routines

idWavs = 1
idDoubles = 2
idSines = 10
idSines3 = 9
idSines2 = 9
idPartials = 9
idSines4 = 19
idBuzzes = 11
idConsts = 17
idLins = 7
idCubes = 6
idExps = 5
idStartEnds = 16
idSplines = 8
idPolys = 3
idChebs1 = 13
idChebs2 = 14
idBessels = 12
idWins = 20
idMp3s = 49
idTabHarmonics = 30
idMixOnTab = 31
idMixTabs = 32

idNormTab = 4
idLinTab = 18

idRandDists = 21
idReadNumFile = 23
idReadNumTab = 24
idExpsBreakPoints = 25
idLinsBreakPoints = 27
idReadTrajectoryFile = 28
idMixSines1 = 33
idMixSines2 = 34
idRandHist = 40
idRandPairs = 41
idRandRanges = 42
idPvocex = 43
idTuning = 51
idMultichannel = 52

idTanh     = "tanh"
idExp      = "exp"
idSone     = "sone"
idFarey    = "farey"
idWave     = "wave"

-- Identifiers for named GEN-routines

idPadsynth, idTanh, idExp, idSone, idFarey, idWave :: Text

idPadsynth = "padsynth"

---------------------------------------------
-- not implemented yet (hard to implement within the current model)

idPolynomFuns = 15

----------------------------------------------------------
-- Jacko

type JackoConnect = (Text, Text)

-- | Describes the Jacko header. All information that is going to be set in the global settings for Jacko opcodes.
-- The jacko opcodes allows us to easily turn our app into Jack-client. We can also do it with command line flags.
-- But the Jacko opcodes provide more options.
--
-- see the Csound docs for details: <http://csound.github.io/docs/manual/JackoOpcodes.html>
data Jacko = Jacko
    { jackoClient       :: Text
    , jackoServer       :: Text
    , jackoAudioIns     :: [JackoConnect]
    , jackoAudioOuts    :: [JackoConnect]
    , jackoMidiIns      :: [JackoConnect]
    , jackoMidiOuts     :: [JackoConnect]
    , jackoFreewheel    :: Bool
    , jackoInfo         :: Bool
    } deriving (Eq, Show, Read)

instance Default Jacko where
    def = Jacko
        { jackoClient       = "csound-exp"
        , jackoServer       = "default"
        , jackoAudioIns     = []
        , jackoAudioOuts    = []
        , jackoMidiIns      = []
        , jackoMidiOuts     = []
        , jackoFreewheel    = False
        , jackoInfo         = False }

renderJacko :: Jacko -> Text
renderJacko spec = Text.unlines $ filter ( /= "")
    [ "JackoInit " <> (Text.pack $ show $ jackoServer spec) <> ", " <> (Text.pack $ show $ jackoClient spec)
    , if (jackoFreewheel spec) then "JackoFreewheel 1" else ""
    , if (jackoInfo spec) then "JackoInfo" else ""
    , renderConnections "JackoAudioInConnect" $ jackoAudioIns spec
    , renderConnections "JackoAudioOutConnect" $ jackoAudioOuts spec
    , renderConnections "JackoMidiInConnect" $ jackoMidiIns spec
    , renderConnections "JackoMidiOutConnect" $ jackoMidiOuts spec
    , "JackoOn" ]
    where
        renderConnections name links = Text.unlines $ fmap (renderLink name) links

        renderLink name (a, b) = name <> " " <> Text.pack (show a) <> ", " <> Text.pack (show b)

csdNeedTrace :: Options -> Bool
csdNeedTrace opt = fromMaybe False $ csdTrace opt

----------------------------------------------------------------------
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
setJack :: Text -> Options
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
setInput :: Text -> Options
setInput a = def { csdFlags = def { audioFileOutput = def { input = Just a } } }

-- | Set's the output name of the device or file.
setOutput :: Text -> Options
setOutput a = def { csdFlags = def { audioFileOutput = def { output = Just a } } }

-- | Provides name identifier for dac.
setDacBy :: Text -> Options
setDacBy port = setOutput name
    where name
            | Text.null port = "dac"
            | otherwise = "dac:" <> port

-- | Provides name identifier for adc.
setAdcBy :: Text -> Options
setAdcBy port = setInput name
    where name
            | Text.null port = "adc"
            | otherwise = "adc:" <> port

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
setMidiDevice :: Text -> Options
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

-- | Defines what ports we should connect after application is launched
--
-- It invokes @jack_connect@ for every pair of port-names in the list.
setJackConnect :: [(Text, Text)] -> Options
setJackConnect connections = def { csdJackConnect = Just connections }

setVirtual :: Options -> Options
setVirtual a = a { csdFlags = (csdFlags a) { rtmidi = Just VirtualMidi, midiRT = m { midiDevice = Just "0" } } }
    where m = midiRT $ csdFlags a

logTrace :: Options -> String
logTrace opt
  | csdNeedTrace opt = ""
  | otherwise        = "--logfile=null"

-- | Saves the user options in the current directory.
--
-- If it's saved in the User's home directory it becomes
-- global options.
saveUserOptions :: Options -> IO ()
saveUserOptions opts = do
    fileName <- fmap rcFileAt getCurrentDirectory
    writeFile fileName (show opts)

getUserOptions :: IO (Maybe Options)
getUserOptions = do
    mHome <- getAt getHomeDirectory
    mCur  <- getAt getCurrentDirectory
    return $ case (mHome, mCur) of
        (_, Just opt) -> Just opt
        (Just opt, Nothing) -> Just opt
        (Nothing, Nothing) -> Nothing
    where
        getAt getPath = do
            fileName <- fmap rcFileAt getPath
            isExist <- doesFileExist fileName
            if isExist
                then do
                    fileContent <- readFile fileName
                    return $ readMaybe fileContent
                else do
                    return Nothing

rcFileAt :: FilePath -> String
rcFileAt dir = dir </> ".csound-expression-rc"
