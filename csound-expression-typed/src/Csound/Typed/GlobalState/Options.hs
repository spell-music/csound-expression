{-# Language CPP #-}
module Csound.Typed.GlobalState.Options (
    Options(..),
    defGain, defSampleRate, defBlockSize, defTabFi, defScaleUI,
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
    Jacko(..), JackoConnect, renderJacko
) where

import Data.Monoid
import Control.Applicative
import Data.Default

import qualified Data.IntMap as IM
import qualified Data.Map    as M

import Csound.Dynamic hiding (csdFlags)

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
    , csdGain           :: Maybe Double             -- ^ A gain of the final output
    , csdTabFi          :: Maybe TabFi              -- ^ Default fidelity of the arrays
    , csdScaleUI        :: Maybe (Double, Double)   -- ^ Scale factors for UI-window
    , csdJacko          :: Maybe Jacko
    } deriving (Eq, Show, Read)

instance Default Options where
    def = Options def def def def def def def

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
    , csdTabFi          = csdTabFi a <|> csdTabFi b
    , csdScaleUI        = csdScaleUI a <|> csdScaleUI b
    , csdJacko          = csdJacko a <|> csdJacko b }

defScaleUI :: Options -> (Double, Double)
defScaleUI = maybe (1, 1) id . csdScaleUI

defGain :: Options -> Double
defGain = maybe 0.8 id . csdGain

defSampleRate :: Options -> Int
defSampleRate = maybe 44100 id . csdSampleRate

defBlockSize :: Options -> Int
defBlockSize = maybe 64 id . csdBlockSize

defTabFi :: Options -> TabFi
defTabFi = maybe def id . csdTabFi

-- | Table size fidelity (how many points in the table by default).
data TabFi = TabFi
    { tabFiBase   :: Int
    , tabFiGens   :: IM.IntMap Int
    , tabNamedFiGens :: M.Map String Int
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
fineFi :: Int -> [(Int, Int)] -> [(String, Int)] -> TabFi
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

idPadsynth, idTanh, idExp, idSone, idFarey, idWave :: String

idPadsynth = "padsynth"

---------------------------------------------
-- not implemented yet (hard to implement within the current model)

idPolynomFuns = 15


----------------------------------------------------------
-- Jacko

type JackoConnect = (String, String)

-- | Describes the Jacko header. All information that is going to be set in the global settings for Jacko opcodes.
-- The jacko opcodes allows us to easily turn our app into Jack-client. We can also do it with command line flags.
-- But the Jacko opcodes provide more options.
--
-- see the Csound docs for details: <http://csound.github.io/docs/manual/JackoOpcodes.html>
data Jacko = Jacko
    { jackoClient       :: String
    , jackoServer       :: String
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

renderJacko :: Jacko -> String
renderJacko spec = unlines $ filter ( /= "")
    [ "JackoInit " ++ (str $ jackoServer spec) ++ ", " ++ (str $ jackoClient spec)
    , if (jackoFreewheel spec) then "JackoFreewheel 1" else ""
    , if (jackoInfo spec) then "JackoInfo" else ""
    , renderConnections "JackoAudioInConnect" $ jackoAudioIns spec
    , renderConnections "JackoAudioOutConnect" $ jackoAudioOuts spec
    , renderConnections "JackoMidiInConnect" $ jackoMidiIns spec
    , renderConnections "JackoMidiOutConnect" $ jackoMidiOuts spec
    , "JackoOn" ]
    where
        renderConnections name links = unlines $ fmap (renderLink name) links

        renderLink name (a, b) = name ++ " " ++ (str a) ++ ", " ++  (str b)

        str x = "\"" ++ x ++ "\""
