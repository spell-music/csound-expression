-- | Creating Function Tables (Buffers)
module Csound.Tab (
    -- | If you are not familliar with Csound's conventions
    -- you are pobably not aware of the fact that for efficiency reasons Csound requires that table size is equal
    -- to power of 2 or power of two plus one which stands for guard point (you do need guard point if your intention is to read the 
    -- table once but you don't need the guard point if you read the table in many cycles, then the guard point is the the first point of your table).  
    Tab, noTab,

    -- * Table querries

    nsamp, ftlen, ftsr, ftchnls, ftcps,

    -- * Table granularity
    TabFi, fineFi, coarseFi,        

    -- * Fill table with numbers
    doubles,
   
    -- * Create new tables to write/update data

    newTab, newGlobalTab, tabSizeSeconds, tabSizePower2, tabSizeSecondsPower2,

    -- * Read from files
    WavChn(..), Mp3Chn(..),
    wavs, wavLeft, wavRight, mp3s, mp3Left, mp3Right, mp3m,
    readNumFile, readTrajectoryFile, readPvocex, readMultichannel,

    -- * (In)Harmonic series
    PartialStrength, PartialNumber, PartialPhase, PartialDC,
    sines, sines3, sines2, sines1, sines4, buzzes, bwSines, bwOddSines,
    mixOnTab, mixTabs,
    tabSines1, tabSines2,

    -- ** Special cases
    sine, cosine, sigmoid, sigmoidRise, sigmoidFall, tanhSigmoid,
    triTab, sawTab, sqrTab, pwTab,
    tanhTab, rescaleTanhTab, expTab, rescaleExpTab, soneTab, rescaleSoneTab,

    -- * Interpolants    
    -- | All funtions have the same shape of arguments:
    --
    -- > fun [a, n1, b, n2, c, ...]
    --
    -- where
    --
    -- * a, b, c .. - are ordinate values
    --
    -- * n1, n2 .. - are lengths of the segments relative to the total number of the points in the table   
    --    
    -- Csounders, Heads up! all segment lengths are relative to the total sum of the segments.
    -- You don't need to make the sum equal to the number of points in the table. Segment's lengths will be resized 
    -- automatically. For example if we want to define a curve that rises to 1 over 25\% of the table and then falls down to zero
    -- we can define it like this:
    --
    -- > lins [0, 0.25, 1, 0.75, 0] 
    --
    -- or
    --
    -- > lins [0, 25, 1, 75, 0]
    --
    -- or
    --
    -- > lins [0, 1, 1, 3, 0]
    --
    -- all these expressions are equivalent. 
    consts, lins, cubes, exps, splines, startEnds, tabseg,
    -- ** Equally spaced interpolants
    econsts, elins, ecubes, eexps, esplines, estartEnds, etabseg,

    -- * Polynomials    
    polys, chebs1, chebs2, bessels,

    -- * Random values 

    -- ** Distributions
    uniDist, linDist, triDist, expDist, biexpDist, gaussDist,
    cauchyDist, pcauchyDist, betaDist, weibullDist, poissonDist,
    tabDist,
    -- *** Distributions with levels
    uniDist', linDist', triDist', expDist', biexpDist', gaussDist',
    cauchyDist', pcauchyDist', betaDist', weibullDist', poissonDist',
    
    -- ** Rand values and ranges
    randDist, rangeDist,


    -- * Windows  
    winHamming, winHanning,  winBartlett, winBlackman,
    winHarris, winGaussian, winKaiser, winRectangle, winSync,

    -- * Padsynth
    padsynth, PadsynthSpec(..), PadsynthShape(..), defPadsynthSpec,

    -- * Harmonics
    tabHarmonics,

    -- * Normalize table
    normTab, NormTabSpec(..), scaleTab,

    -- * Low level Csound definition.
    gen,
    
    -- * Modify tables
    skipNorm, forceNorm, setSize, setDegree, guardPoint, gp,
    
    -- ** Handy shortcuts        
    -- | handy shortcuts for the function 'setDegree'.
    lllofi, llofi, lofi, midfi, hifi, hhifi, hhhifi,
    
    -- * Identifiers for GEN-routines
    
    -- | Low level Csound integer identifiers for tables. These names can be used in the function 'Csound.Base.fineFi'
    idWavs, idMp3s, idDoubles, idSines, idSines3, idSines2, 
    idPartials, idSines4, idBuzzes, idConsts, idLins, idCubes, 
    idExps, idSplines, idStartEnds,  idPolys, idChebs1, idChebs2, idBessels, idWins,
    idPadsynth, idTanh, idExp, idSone, idFarey, idWave,

    -- * Tabular opcodes
    sec2rel,

    -- * Tables of tables
    TabList, tabList, fromTabList, fromTabListD,

    -- * Mic table functions
    tablewa, tablew, readTab, readTable, readTable3, readTablei,

    -- ** Table Reading with Dynamic Selection
    tableikt, tablekt, tablexkt,

    -- ** random generators from tables
    cuserrnd, duserrnd
) where

import Control.Applicative hiding ((<*))
import Control.Arrow(second)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Csound.Dynamic hiding (int, when1, whens)

import Data.Default
import Csound.Typed
import Csound.Typed.Opcode(ftgentmp, ftgenonce)
import Data.Maybe

-- | The default table. It's rendered to @(-1)@ in the Csound.
noTab :: Tab
noTab = fromE (-1)

{-
-- | Creates a new table. The Tab could be used while the instrument
-- is playing. When the instrument is retriggered the new tab is allocated.
--
-- > newTab size
newTab :: D -> SE Tab
newTab size = ftgentmp 0 0 size 7 0 [size, 0]

-- | Creates a new global table. 
-- It's generated only once. It's persisted between instrument calls.
--
-- > newGlobalTab identifier size
newGlobalTab :: D -> SE Tab
newGlobalTab size = do  
    identifier <- getNextGlobalGenId
    ref <- newGlobalRef (0 :: D)        
    tabId <- ftgenonce 0 (int identifier) size 7 0 [size, 0]
    writeRef ref (fromGE $ toGE tabId)
    fmap (fromGE . toGE) $ readRef ref
-}

-- | Calculates the number of samples needed to store the given amount of seconds.
-- It multiplies the value by the current sample rate.
tabSizeSeconds :: D -> D
tabSizeSeconds x = x * getSampleRate

-- | Calculates the closest power of two value for a given size.
tabSizePower2 :: D -> D
tabSizePower2 x = 2 ** (ceil' $ logBase 2 x)

-- | Calculates the closest power of two value in samples for a given size in seconds.
tabSizeSecondsPower2 :: D -> D
tabSizeSecondsPower2 = tabSizePower2 . tabSizeSeconds

data WavChn = WavLeft | WavRight | WavAll
    deriving (Show, Eq)


instance Default WavChn where
    def = WavAll

fromWavChn :: WavChn -> Int
fromWavChn x = case x of
    WavAll   -> 0
    WavLeft  -> 1
    WavRight -> 2

-- | Loads wav or aiff file to table
--
-- > wavs fileName skipTime channel
--
-- skipTime specifies from what second it should read the file.
--
-- with channel argument we can read left, right or both channels.
wavs :: String -> Double -> WavChn -> Tab
wavs filename skiptime channel = preTab (SizePlain 0) idWavs 
    (FileAccess filename [skiptime, format, fromIntegral $ fromWavChn channel])
    where format = 0

data Mp3Chn = Mp3Mono | Mp3Stereo | Mp3Left | Mp3Right | Mp3All
    deriving (Show, Eq)

fromMp3Chn :: Mp3Chn -> Int
fromMp3Chn x = case x of
    Mp3Mono     -> 1
    Mp3Stereo   -> 2
    Mp3Left     -> 3
    Mp3Right    -> 4
    Mp3All      -> 0

instance Default Mp3Chn where
    def = Mp3All

-- | Reads left channel of audio-file
wavLeft :: String -> Tab
wavLeft file = wavs file 0 WavLeft

-- | Reads right channel of audio-file
wavRight :: String -> Tab
wavRight file = wavs file 0 WavRight

-- | Loads mp3 file to table:
--
-- > mp3s fileName skipTime format
--
-- skipTime specifies from what second it should read the file.
-- 
-- format is: 1 - for mono files, 2 - for stereo files, 3 - for left channel of stereo file,
-- 4 for right channel of stereo file
mp3s :: String -> Double -> Mp3Chn -> Tab
mp3s filename skiptime channel = preTab (SizePlain 0) idMp3s 
    (FileAccess filename [skiptime, format])
    where format = fromIntegral $ fromMp3Chn channel

-- | Reads left channel of mp3-file
mp3Left :: String -> Tab
mp3Left file = mp3s file 0 Mp3Left

-- | Reads right channel of mp3-file
mp3Right :: String -> Tab
mp3Right file = mp3s file 0 Mp3Right

-- | Reads mono of mp3-file
mp3m :: String -> Tab
mp3m file = mp3s file 0 Mp3Mono

interp :: Int -> [Double] -> Tab
interp genId as = preTab def genId (relativeArgs as)

plains :: Int -> [Double] -> Tab
plains genId as = preTab def genId (ArgsPlain $ return as)

insertOnes :: [Double] -> [Double]
insertOnes xs = case xs of
    [] -> []
    a:[] -> [a]
    a:as -> a : 1 : insertOnes as

findTableSize :: Int -> Int
findTableSize n
    | isPowerOfTwo n        = n
    | isPowerOfTwo (n - 1)  = n
    | otherwise             = -n
    
isPowerOfTwo :: Int -> Bool
isPowerOfTwo a 
    | null zeroes   = False
    | otherwise     = all ( == 0) zeroes
    where zeroes = fmap (flip mod 2) $ takeWhile (> 1) $ iterate (\x -> div x 2) a

-- loadFile :: Int -> String -> Double -> Tab

-- | Table contains all provided values 
-- (table is extended to contain all values and to be of the power of 2 or the power of two plus one).
-- (by default it skips normalization).
doubles :: [Double] -> Tab
doubles as = skipNorm $ setSize (findTableSize n) $ plains idDoubles as
    where n = length as

-- | Segments of the exponential curves.
--
-- > exps [a, n1, b, n2, c, ...]
--
-- where 
-- 
-- * @a, b, c, ...@ are ordinate values
--
-- * @n1, n2, ...@  are lengths of the segments relative to the total number of the points in the table
exps :: [Double] -> Tab
exps = interp idExps

-- | Equally spaced segments of exponential curves.
--
-- > eexps [a, b, c, ...] 
--
-- is the same as
--
-- > exps [a, 1, b, 1, c, ...]
eexps :: [Double] -> Tab
eexps = exps . insertOnes

-- | Segments of cubic polynomials. 
--
-- > cubes [a, n1, b, n2, c, ...]
--
-- where
--
-- * a, b, c .. - are ordinate values
--
-- * @n1, n2, ...@  are lengths of the segments relative to the total number of the points in the table
cubes :: [Double] -> Tab
cubes = interp idCubes

-- | Equally spaced segments of cubic polynomials.
--
-- > ecubes [a, b, c, ...] 
--
-- is the same as
--
-- > cubes [a, 1, b, 1, c, ...]
ecubes :: [Double] -> Tab
ecubes = cubes . insertOnes

-- | Segments of straight lines. 
--
-- > lins [a, n1, b, n2, c, ...]
--
-- where
--
-- * a, b, c .. - are ordinate values
--
-- * @n1, n2, ...@  are lengths of the segments relative to the total number of the points in the table
lins :: [Double] -> Tab
lins = interp idLins

-- | Equally spaced segments of straight lines.
--
-- > elins [a, b, c, ...] 
--
-- is the same as
--
-- > lins [a, 1, b, 1, c, ...]
elins :: [Double] -> Tab
elins = lins . insertOnes

-- | Cubic spline curve.
--
-- > splines [a, n1, b, n2, c, ...]
--
-- where
--
-- * a, b, c .. - are ordinate values
--
-- * @n1, n2, ...@  are lengths of the segments relative to the total number of the points in the table
splines :: [Double] -> Tab
splines = interp idSplines

-- | Equally spaced spline curve.
--
-- > esplines [a, b, c, ...] 
--
-- is the same as
--
-- > splines [a, 1, b, 1, c, ...]
esplines :: [Double] -> Tab
esplines = splines . insertOnes

-- | Constant segments (sample and hold).
--
-- > consts [a, n1, b, n2, c, ...]
--
-- where
--
-- * a, b, c .. - are ordinate values
--
-- * @n1, n2, ...@  are lengths of the segments relative to the total number of the points in the table
consts :: [Double] -> Tab
consts = interp idConsts

-- | Equally spaced constant segments.
--
-- > econsts [a, b, c, ...] 
--
-- is the same as
--
-- > consts [a, 1, b, 1, c, ...]
econsts :: [Double] -> Tab
econsts = consts . insertOnes
   
-- | Creates a table from a starting value to an ending value.
--
-- > startEnds [val1, dur1, type1, val2, dur2, type2, val3, ... typeX, valN]
--
-- * val1, val2 ... -- end points of the segments
--
-- * dur1, dur2 ... -- durations of the segments
--
-- * type1, type2 ... -- if 0, a straight line is produced. If non-zero, then it creates the following curve, for dur steps:
--
-- > beg + (end - beg) * (1 - exp( i*type)) / (1 - exp(type * dur))
-- 
-- * beg, end - end points of the segment
--
-- * dur - duration of the segment
startEnds :: [Double] -> Tab
startEnds as = preTab def idStartEnds (relativeArgsGen16 as)

-- | Equally spaced interpolation for the function @startEnds@
--
-- > estartEnds [val1, type1, val2, typ2, ...]
--
-- is the same as
--
-- > estartEnds [val1, 1, type1, val2, 1, type2, ...]
estartEnds :: [Double] -> Tab
estartEnds = startEnds . insertOnes16
    where 
        insertOnes16 xs = case xs of
            a:b:as  -> a : 1 : b : insertOnes16 as
            _       -> xs

type PartialNumber = Double
type PartialStrength = Double
type PartialPhase = Double
type PartialDC = Double

-- | Series of harmonic partials:
--
-- > sine = sines [1]
--
-- > saw = sines $ fmap (1 / ) [1 .. 10]
--
-- > square = sines $ fmap (1 / ) [1, 3 .. 11]
--
-- > triangle = sines $ zipWith (\a b -> a / (b ** 2)) (cycle [1, -1]) [1, 3 .. 11]
sines :: [PartialStrength] -> Tab
sines = plains idSines

-- | Just like 'Csound.Tab.sines2' but partial strength is set to one.
sines1 :: [PartialNumber] -> Tab
sines1 xs = sines2 $ zip xs (repeat 1)

-- | Just like 'Csound.Tab.sines3' but phases are set to zero.
sines2 :: [(PartialNumber, PartialStrength)] -> Tab
sines2 xs = sines3 [(num, strength, 0) | (num, strength) <- xs]

-- | Specifies series of possibly inharmonic partials.
sines3 :: [(PartialNumber, PartialStrength, PartialPhase)] -> Tab
sines3 xs = plains idSines3 [a | (pn, strength, phs) <- xs, a <- [pn, strength, phs]]

-- | Specifies series of possibly inharmonic partials with direct current.
sines4 :: [(PartialNumber, PartialStrength, PartialPhase, PartialDC)] -> Tab
sines4 xs = plains idSines4 [a | (pn, strength, phs, dc) <- xs, a <- [pn, strength, phs, dc]]

-- | Sines with bandwidth (simplified padsynth generator)
--
-- bwSines harmonics bandwidth
bwSines :: [Double] -> Double -> Tab
bwSines harmonics bandwidth = padsynth (defPadsynthSpec bandwidth harmonics)

-- | Sines with bandwidth (simplified padsynth generator). Only odd harmonics are present
--
-- bwOddSines harmonics bandwidth
bwOddSines :: [Double] -> Double -> Tab
bwOddSines harmonics bandwidth = padsynth ((defPadsynthSpec bandwidth harmonics) { padsynthHarmonicStretch = 2 })


-- | Table for pure sine wave.
sine :: Tab
sine = sines [1]

-- | Table for pure cosine wave.
cosine :: Tab
cosine = buzzes 1 []

-- | Table for sigmoid wave.
sigmoid :: Tab
sigmoid = sines4 [(0.5, 0.5, 270, 0.5)]

-- | Table for sigmoid rise wave.
sigmoidRise :: Tab
sigmoidRise = guardPoint $ sines4 [(0.5, 1, 270, 1)]

-- | Table for sigmoid fall wave.
sigmoidFall :: Tab
sigmoidFall = guardPoint $ sines4 [(0.5, 1, 90, 1)]

-- | Creates tanh sigmoid. The argument is the radius of teh sigmoid.
tanhSigmoid :: Double -> Tab
tanhSigmoid x = esplines (fmap tanh [-x, (-x +0.5) .. x]) 

-- | Generates values similar to the opcode 'Csound.Opcode.Basic.buzz'. 
--
-- > buzzes numberOfHarmonics [lowestHarmonic, coefficientOfAttenuation]
--
-- With @buzzes n [l, r]@ you get @n@ harmonics from @l@ that are attenuated by the factor of @r@
-- on each step.
buzzes :: Double -> [Double] -> Tab
buzzes nh opts = plains idBuzzes (nh : take 2 opts)

-- | Modified Bessel function of the second kind, order 0 (for amplitude modulated FM). 
--
-- > bessels xint
--
-- the function is defined within the interval @[0, xint]@.
bessels :: Double -> Tab
bessels xint = plains idBessels [xint]

-- | Polynomials.
--
-- > polys xl xr [c0, c1, c2, ..]
--
-- where
--
-- * xl, xr - left and right values of the interval over wich polynomial is defined
--
-- * [c0, c1, c2, ...] -- coefficients of the polynomial
--
-- > c0 + c1 * x + c2 * x * x + ...
polys :: Double -> Double -> [Double] -> Tab
polys x0 x1 cs = plains idPolys (x0:x1:cs)

-- | Chebyshev polynomials of the first kind.
--
-- > polys xl xr [h0, h1, h2, ..]
--
-- where
--
-- * xl, xr - left and right values of the interval over wich polynomial is defined
--
-- * [h0, h1, h2, ...] -- relative strength of the partials
chebs1 :: Double -> Double -> [Double] -> Tab
chebs1 xint xamp hs = plains idChebs1 (xint : xamp : hs)

-- | Chebyshev polynomials of the second kind.
--
-- > polys xl xr [h0, h1, h2, ..]
--
-- where
--
-- * xl, xr - left and right values of the interval over wich polynomial is defined
--
-- * [h0, h1, h2, ...] -- relative strength of the partials
chebs2 :: Double -> Double -> [Double] -> Tab
chebs2 xint xamp hs = plains idChebs2 (xint : xamp : hs)

winHamming, winHanning, winBartlett, winBlackman,
    winHarris, winGaussian, winKaiser, winRectangle, winSync :: [Double] -> Tab


winHamming      = wins Hamming
winHanning      = wins Hanning
winBartlett     = wins Bartlett
winBlackman     = wins Blackman
winHarris       = wins Harris
winRectangle    = wins Rectangle
winSync         = wins Sync
winGaussian     = wins Gaussian
winKaiser       = wins Kaiser

data WinType 
    = Hamming | Hanning | Bartlett | Blackman
    | Harris | Gaussian | Kaiser | Rectangle | Sync

winTypeId :: WinType -> Double
winTypeId x = case x of
    Hamming     -> 1
    Hanning     -> 2
    Bartlett    -> 3
    Blackman    -> 4
    Harris      -> 5
    Gaussian    -> 6
    Kaiser      -> 7
    Rectangle   -> 8
    Sync        -> 9

wins :: WinType -> [Double] -> Tab
wins ty params = gen idWins (winTypeId ty : params)

-- | Padsynth parameters.
--
-- see for details: <http://csound.github.io/docs/manual/GENpadsynth.html>
data PadsynthSpec = PadsynthSpec 
    { padsynthFundamental     :: Double
    , padsynthBandwidth       :: Double    
    , padsynthPartialScale    :: Double
    , padsynthHarmonicStretch :: Double
    , padsynthShape           :: PadsynthShape
    , padsynthShapeParameter  :: Double
    , padsynthHarmonics       :: [Double]
    } deriving (Show, Eq)

data PadsynthShape = GaussShape | SquareShape | ExpShape
    deriving (Show, Eq, Ord, Enum)

padsynthShapeId :: PadsynthShape -> Double
padsynthShapeId shape = fromIntegral $ 1 + (fromEnum shape)

-- | Specs for padsynth algorithm:
--
-- > defPadsynthSpec partialBandwidth harmonics
--
-- * partialBandwidth -- bandwidth of the first partial.
--
-- * harmonics -- the list of amplitudes for harmonics.
defPadsynthSpec :: Double -> [Double] -> PadsynthSpec
defPadsynthSpec partialBW harmonics = PadsynthSpec 261.625565 partialBW 1 1 GaussShape 1 harmonics

-- | Creates tables for the padsynth algorithm (described at <http://www.paulnasca.com/algorithms-created-by-me>).
-- The table size should be very big the default is 18 power of 2.
-- 
-- csound docs: <http://csound.github.io/docs/manual/GENpadsynth.html>
padsynth :: PadsynthSpec -> Tab
padsynth (PadsynthSpec fundamentalFreq partialBW partialScale harmonicStretch shape shapeParameter harmonics) = 
    plainStringTab idPadsynth ([fundamentalFreq, partialBW, partialScale, harmonicStretch, padsynthShapeId shape, shapeParameter] ++ harmonics)

                                    -- 261.625565     25.0         1.0             1.0             2.0                 1.0             1.0 0.5 0.0 0.2

plainStringTab :: String -> [Double] -> Tab
plainStringTab genId as = preStringTab def genId (ArgsPlain $ return as)

-- | Creates a table of doubles (It's f-table in Csound).
-- Arguments are:
--
-- * identificator of the GEN routine
--
-- * GEN routine arguments
--
-- All tables are created at 0 and memory is never released.
gen :: Int -> [Double] -> Tab
gen genId args = preTab def genId (ArgsPlain $ return args)

-- | Adds guard point to the table size (details of the interpolation schemes: you do need guard point if your intention is to read the 
-- table once but you don't need the guard point if you read table in many cycles, the guard point is the the first point of your table).  
guardPoint :: Tab -> Tab
guardPoint = updateTabSize $ \x -> case x of
    SizePlain n -> SizePlain $ plainGuardPoint n
    a -> a{ hasGuardPoint = True }    
    where plainGuardPoint n
            | even n    = n + 1
            | otherwise = n

-- | Shortcut for 'Csound.Tab.guardPoint'.
gp :: Tab -> Tab
gp = guardPoint

-- | Sets an absolute size value. As you can do it in the Csound files.
setSize :: Int -> Tab -> Tab
setSize n = updateTabSize $ const (SizePlain n)

-- | Sets the relative size value. You can set the base value in the options 
-- (see 'Csound.Base.tabResolution' at 'Csound.Base.CsdOptions', with tabResolution you can easily change table sizes for all your tables).
-- Here zero means the base value. 1 is the base value multiplied by 2, 2 is the base value multiplied by 4
-- and so on. Negative values mean division by the specified degree. 
setDegree :: Int -> Tab -> Tab
setDegree degree = updateTabSize $ \x -> case x of
    SizePlain n -> SizePlain n
    a -> a{ sizeDegree = degree }

-- | Sets degrees from -3 to 3.
lllofi, llofi, lofi, midfi, hifi, hhifi, hhhifi :: Tab -> Tab 

lllofi  = setDegree (-3)
llofi   = setDegree (-2)
lofi    = setDegree (-1)
midfi   = setDegree 0
hifi    = setDegree 1
hhifi   = setDegree 2
hhhifi  = setDegree 3 

-- | Writes tables in sequential locations.
--
-- This opcode writes to a table in sequential locations to and from an a-rate 
-- variable. Some thought is required before using it. It has at least two major, 
-- and quite different, applications which are discussed below.
--
-- > kstart tablewa kfn, asig, koff
--
-- csound docs: <http://www.csounds.com/manual/html/tablewa.html>
tablewa ::  Tab -> Sig -> Sig -> SE Sig
tablewa b1 b2 b3 = fmap (Sig . return) $ SE $ (depT =<<) $ lift $ f <$> unTab b1 <*> unSig b2 <*> unSig b3
    where f a1 a2 a3 = opcs "tablewa" [(Kr,[Kr,Ar,Kr])] [a1,a2,a3]


-- | Transforms phasor that is defined in seconds to relative phasor that ranges in 0 to 1.
sec2rel :: Tab -> Sig -> Sig
sec2rel tab x = x / (sig $ ftlen tab / getSampleRate)

---------------------------------------------------

-- | Generates harmonic partials by analyzing an existing table.
--
-- > tabHarmonics src minh maxh [ref_sr] [interp]
--
-- * src -- source ftable. It should be primitive ie constructed not with "ftgen" family of opcodes.
--
-- * minh -- lowest harmonic number
--
-- * maxh -- maxh -- highest harmonic number
--
-- * ref_sr (optional) -- maxh is scaled by (sr / ref_sr). The default value of ref_sr is sr. If ref_sr is zero or negative, it is now ignored. 
--
-- * interp (optional) -- if non-zero, allows changing the amplitude of the lowest and highest harmonic partial depending on the fractional part of minh and maxh. For example, if maxh is 11.3 then the 12th harmonic partial is added with 0.3 amplitude. This parameter is zero by default.
--
-- GEN30 for Csound: <http://www.csounds.com/manual/html/GEN30.html>
--
tabHarmonics :: Tab -> Double -> Double -> Maybe Double -> Maybe Double -> Tab
tabHarmonics tab minh maxh mrefSr mInterp = hideGE $ do
    idx <- renderTab tab
    return $ preTab def idTabHarmonics (ArgsPlain $ return (catMaybes $ fmap Just [fromIntegral idx, minh, maxh] ++ [mrefSr, mInterp]))

---------------------------------
-- mixing tabs GEN31 GEN32

-- | It's just like sines3 but inplace of pure sinewave it uses supplied in the first argument shape.
-- 
-- mixOnTab srcTable [(partialNumber, partialStrength, partialPahse)]
--
-- phahse is in range [0, 1]
mixOnTab :: Tab -> [(PartialNumber, PartialStrength, PartialPhase)] -> Tab
mixOnTab tab xs = hideGE $ do
    idx <- renderTab tab
    return $ plains idMixOnTab $ fromIntegral idx : [a | (pn, strength, phs) <- xs, a <- [pn, strength, phs]]

-- | It's like @mixOnTab@ but it's more generic since we can mix not only one shape.
-- But we can specify shape for each harmonic.
mixTabs  :: [(Tab, PartialNumber, PartialStrength, PartialPhase)] -> Tab
mixTabs xs = hideGE $ do
    args <- sequence [a | (tab, pn, strength, phs) <- xs, a <- (fmap fromIntegral $ renderTab tab) : fmap return [pn, strength, phs]]
    return $ plains idMixTabs args

--  | Normalizing table
--
-- Csound GEN04: <http://www.csounds.com/manual/html/GEN04.html>
normTab :: NormTabSpec -> Tab -> Tab
normTab spec tab = hideGE $ do
    idx <- renderTab tab
    return $ plains idNormTab $ fmap fromIntegral [idx, fromNormTabSpec spec]
    where
        fromNormTabSpec x = case x of
            ScanLeftToRight -> 0
            ScanFromMiddle  -> 1

data NormTabSpec = ScanLeftToRight | ScanFromMiddle

-- | Creates a new table wich contains all values from the source table rescaled to the given interval.
--
-- > scaleTab (minValue, maxValue) sourceTab
scaleTab :: (Double, Double) -> Tab -> Tab
scaleTab (minVal, maxVal) tab = hideGE $ do
    tabId <- renderTab tab
    return $ skipNorm $ gen idReadNumTab [fromIntegral tabId, minVal, maxVal]

----------------------------------------------------

-- | tabseg  -- Writes composite waveforms made up of pre-existing waveforms. 
--
-- tabseg [(tab, amplitude, duration)]
--
-- Csound GEN18: <http://www.csounds.com/manual/html/GEN18.html>
--
-- Butnotice the difference with Csound we specify start and finish of writing but
-- here we only specify the relative length of segments. Segments are arranged so
-- that the start f next segment comes right after the end of the prev segment.
tabseg :: [(Tab, PartialStrength, Double)] -> Tab
tabseg xs = hideGE $ do 
    tabIds <- mapM renderTab tabs
    return $ preTab def idLinTab $ mkArgs tabIds
    where
        (tabs, amps, durs) = unzip3 xs        
        segments n = fmap (second $ \x -> x - 1) $ tail $ scanl (\(a, b) x -> (b, b + x)) (0, 0) $ mkRelative n durs
        mkArgs ids = ArgsPlain $ reader $ \size -> concat $ zipWith3 (\tabId amp (start, finish) -> [fromIntegral tabId, amp, start, finish]) ids amps (segments size)

etabseg :: [(Tab, PartialStrength)] -> Tab
etabseg = tabseg . fmap (\(tab, amp) -> (tab, amp, 1))

--------------------------------------------------
-- distributions


{- Csound Docs, distribution types

    1 = Uniform (positive numbers only)

    2 = Linear (positive numbers only)

    3 = Triangular (positive and negative numbers)

    4 = Exponential (positive numbers only)

    5 = Biexponential (positive and negative numbers)

    6 = Gaussian (positive and negative numbers)

    7 = Cauchy (positive and negative numbers)

    8 = Positive Cauchy (positive numbers only)

    9 = Beta (positive numbers only)

    10 = Weibull (positive numbers only)

    11 = Poisson (positive numbers only)
-}

gen21 :: Int -> [Double] -> Tab
gen21 typeId aux = gen idRandDists $ fromIntegral typeId : aux

dist :: Int -> Tab
dist n = gen21 n []

-- | Uniform (positive numbers only)
uniDist :: Tab
uniDist = dist 1

-- | Linear (positive numbers only)
linDist :: Tab
linDist = dist 2

-- | Triangular (positive and negative numbers)
triDist :: Tab
triDist = dist 3

-- | Exponential (positive numbers only)
expDist :: Tab
expDist = dist 4

-- | Biexponential (positive and negative numbers)
biexpDist :: Tab
biexpDist = dist 5

-- | Gaussian (positive and negative numbers)
gaussDist :: Tab
gaussDist = dist 6

-- | Cauchy (positive and negative numbers)
cauchyDist :: Tab
cauchyDist = dist 7

-- | Positive Cauchy (positive numbers only)
pcauchyDist :: Tab
pcauchyDist = dist 8

-- | Beta (positive numbers only)
--
-- > betaDist alpha beta
-- 
-- * @alpha@ -- alpha value. If kalpha is smaller than one, smaller values favor values near 0. 
--
-- * @beta@ -- beta value. If kbeta is smaller than one, smaller values favor values near krange.
betaDist :: Double -> Double -> Tab
betaDist arg1 arg2 = gen21 9 [1, arg1, arg2]

-- | Weibull (positive numbers only)
--
-- * tau -- if greater than one, numbers near ksigma are favored. If smaller than one, small values are favored. If t equals 1, the distribution is exponential. Outputs only positive numbers.
weibullDist :: Double -> Tab
weibullDist arg1 = gen21 10 [1, arg1]

-- | Poisson (positive numbers only)
poissonDist :: Tab
poissonDist = dist 11

-- with level

dist' :: Int -> Double -> Tab
dist' n level = gen21 n [level]

uniDist' :: Double -> Tab
uniDist' = dist' 1

linDist' :: Double -> Tab
linDist' = dist' 2

triDist' :: Double -> Tab
triDist' = dist' 3

expDist' :: Double -> Tab
expDist' = dist' 4

biexpDist' :: Double -> Tab
biexpDist' = dist' 5

gaussDist' :: Double -> Tab
gaussDist' = dist' 6

cauchyDist' :: Double -> Tab
cauchyDist' = dist' 7

pcauchyDist' :: Double -> Tab
pcauchyDist' = dist' 8

betaDist' :: Double -> Double -> Double -> Tab
betaDist' level arg1 arg2 = gen21 9 [level, arg1, arg2]

weibullDist' :: Double -> Double -> Tab
weibullDist' level arg1 = gen21 10 [level, arg1]

poissonDist' :: Double -> Tab
poissonDist' = dist' 11

-- GEN40

-- | Generates a random distribution using a distribution histogram (GEN40).
--
-- Csound docs: <http://www.csounds.com/manual/html/GEN40.html>
tabDist :: Tab -> Tab
tabDist src = hideGE $ do
    tabId <- renderTab src
    return $ gen idRandHist [fromIntegral tabId]

-- | randDist — Generates a random list of numerical pairs (GEN41).
--
-- > randDist  [value1, prob1, value2, prob2, value3, prob3 ... valueN, probN]
--
-- The first number of each pair is a value, and the second is the probability of that value to 
-- be chosen by a random algorithm. Even if any number can be assigned to the probability element of each pair, 
-- it is suggested to give it a percent value, in order to make it clearer for the user.
--
-- This subroutine is designed to be used together with duserrnd and urd opcodes (see duserrnd for more information).
randDist :: [Double] -> Tab
randDist xs = skipNorm $ gen idRandPairs xs


-- | rangeDist — Generates a random distribution of discrete ranges of values (GEN42).
--
-- The first number of each group is a the minimum value of the 
-- range, the second is the maximum value and the third is the probability 
-- of that an element belonging to that range of values can be chosen by 
-- a random algorithm. Probabilities for a range should be a fraction of 1, 
-- and the sum of the probabilities for all the ranges should total 1.0.
--
-- This subroutine is designed to be used together with duserrnd and urd opcodes (see duserrnd for more information). 
-- Since both duserrnd and urd do not use any interpolation, it is suggested to give a size reasonably big.
rangeDist :: [Double] -> Tab
rangeDist xs = skipNorm $ gen idRandRanges xs

------------------------------------------------------

-- | Reads numbers from file (GEN23)
readNumFile :: String -> Tab
readNumFile filename = skipNorm $ preTab def idReadNumFile $ FileAccess filename []

-- | Reads trajectory from file (GEN28)
readTrajectoryFile :: String -> Tab
readTrajectoryFile filename = skipNorm $ preTab def idReadTrajectoryFile $ FileAccess filename []

-- | Reads PVOCEX files (GEN43)
readPvocex :: String -> Int -> Tab
readPvocex filename channel = preTab def idPvocex $ FileAccess filename [fromIntegral channel]

-- | readMultichannel — Creates an interleaved multichannel table from the specified source tables, in the format expected by the ftconv opcode (GEN52).
-- 
-- > f # time size 52 nchannels fsrc1 offset1 srcchnls1 [fsrc2 offset2 srcchnls2 ... fsrcN offsetN srcchnlsN]
--
-- csound doc: <http://www.csounds.com/manual/html/GEN52.html>
readMultichannel :: Int -> [(Tab, Int, Int)] -> Tab
readMultichannel n args = hideGE $ do
    idSrcs <- mapM renderTab fsrcs
    return $ skipNorm $ gen idMultichannel $ fmap fromIntegral $ n : (concat $ zipWith3 (\a b c -> [a, b, c]) idSrcs offsets chnls)
    where
        (fsrcs, offsets, chnls) = unzip3 args

------------------------------------------------------

-- | Csound's GEN33 — Generate composite waveforms by mixing simple sinusoids.  
--
-- > tabSines1 srcTab nh scl [fmode]
--
-- Csound docs: <http://www.csounds.com/manual/html/GEN33.html>
tabSines1 :: Tab -> Double -> Double -> Maybe Double -> Tab
tabSines1 = tabSinesBy idMixSines2

-- | Csound's GEN34 — Generate composite waveforms by mixing simple sinusoids. 
--
-- > tabSines2 srcTab nh scl [fmode]
--
-- Csound docs: <http://www.csounds.com/manual/html/GEN3.html>
tabSines2 :: Tab -> Double -> Double -> Maybe Double -> Tab
tabSines2 = tabSinesBy idMixSines2

tabSinesBy :: Int -> Tab -> Double -> Double -> Maybe Double -> Tab
tabSinesBy genId tab nh amp fmode = hideGE $ do
    tabId <- renderTab tab
    return $ preTab def genId $ ArgsPlain $ return $ [fromIntegral tabId, nh, amp] ++ (maybe [] return fmode)

-------------------
-- specific tabs

-- | Linear segments that form a singl cycle of triangle wave.
triTab :: Tab
triTab = elins [0, 1, 0, -1, 0]

-- | Linear segments that form a single cycle of sawtooth wave.
sawTab :: Tab
sawTab = elins [1, -1]

-- | Linear segments that form a single cycle of square wave.
sqrTab :: Tab
sqrTab = lins [1, 0.5, 1, 0.01, -1, 0.5, -1, 0.01, 1]

-- | Pulse-width wave formed with linear segments. Duty cycle rages from 0 to 1. 0.5 is a square wave.
pwTab :: Double -> Tab
pwTab duty = lins [1, duty, 1, 0.01, -1, 1 - duty, -1, 0.01, 1]

-- | Tab with tanh from the given interval.
--
-- > tanhTab (start, end)
tanhTab :: (Double, Double) -> Tab
tanhTab (start, end) = plainStringTab idTanh [start, end, 0]

-- | Tab with tanh from the given interval. The table is rescaled.
--
-- > rescaleTanhTab (start, end)
rescaleTanhTab :: (Double, Double) -> Tab
rescaleTanhTab (start, end) = plainStringTab idTanh [start, end, 1]

-- | Tab with exponential from the given interval.
--
-- > expTab (start, end)
expTab :: (Double, Double) -> Tab
expTab (start, end) = plainStringTab idExp [start, end, 0]

-- | Tab with exponential from the given interval. The table is rescaled.
--
-- > rescaleExpTab (start, end)
rescaleExpTab :: (Double, Double) -> Tab
rescaleExpTab (start, end) = plainStringTab idExp [start, end, 1]

-- | Tab with sone from the given interval.
--
-- > soneTab (start, end) equalpoint
--
-- * start, end -- first and last value to be stored. The points stored are uniformly spaced between these to the table size.
--
-- * equalpoint -- the point on the curve when the input and output values are equal.
soneTab :: (Double, Double) -> Double -> Tab
soneTab (start, end) equalpoint = plainStringTab idSone [start, end, equalpoint, 0]


-- | Tab with sone from the given interval.
--
-- > soneTab (start, end) equalpoint
--
-- * start, end -- first and last value to be stored. The points stored are uniformly spaced between these to the table size.
--
-- * equalpoint -- the point on the curve when the input and output values are equal.
rescaleSoneTab :: (Double, Double) -> Double -> Tab
rescaleSoneTab (start, end) equalpoint = plainStringTab idSone [start, end, equalpoint, 0]

---------------------------------------------------

-- | 
-- tablew — Change the contents of existing function tables. 
--
-- This opcode operates on existing function tables, changing their contents. 
-- tablew is for writing at k- or at a-rates, with the table number being 
-- specified at init time. Using tablew with i-rate signal and index values 
-- is allowed, but the specified data will always be written to the function 
-- table at k-rate, not during the initialization pass. The valid combinations 
-- of variable types are shown by the first letter of the variable names. 
--
-- > tablew asig, andx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- > tablew isig, indx, ifn [, ixmode] [, ixoff] [, iwgmode]
-- > tablew ksig, kndx, ifn [, ixmode] [, ixoff] [, iwgmode]
--
-- csound doc: <http://www.csounds.com/manual/html/tablew.html>
tablew ::  Sig -> Sig -> Tab -> SE ()
tablew b1 b2 b3 = SE $ (depT_ =<<) $ lift $ f <$> unSig b1 <*> unSig b2 <*> unTab b3
    where f a1 a2 a3 = opcs "tablew" [(Xr,[Xr,Xr,Ir,Ir,Ir,Ir])] [a1,a2,a3]


-- | 
-- Notice that this function is the same as @tab@, but it wraps the output in the SE-monad.
-- So you can use the @tab@ if your table is read-only and you can use @readTab@ if
-- you want to update the table and the order of read/write operation is important.
--
-- Fast table opcodes.
--
-- Fast table opcodes. Faster than
--     table and
--     tablew because don't
--     allow wrap-around and limit and don't check index validity. Have
--     been implemented in order to provide fast access to
--     arrays. Support non-power of two tables (can be generated by any
--     GEN function by giving a negative length value).
--
-- > kr  tab  kndx, ifn[, ixmode]
-- > ar  tab  xndx, ifn[, ixmode]
--
-- csound doc: <http://www.csounds.com/manual/html/tab.html>
readTab ::  Sig -> Tab -> SE Sig
readTab b1 b2 = fmap ( Sig . return) $ SE $ (depT =<<) $ lift $ f <$> unSig b1 <*> unTab b2
    where f a1 a2 = opcs "tab" [(Kr,[Kr,Ir,Ir]),(Ar,[Xr,Ir,Ir])] [a1,a2]



-- | 
-- Notice that this function is the same as @table@, but it wraps the output in the SE-monad.
-- So you can use the @table@ if your table is read-only and you can use @readTable@ if
-- you want to update the table and the order of read/write operation is important.
--
-- Accesses table values by direct indexing.
--
-- > ares  table  andx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > ires  table  indx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > kres  table  kndx, ifn [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://www.csounds.com/manual/html/table.html>
readTable :: SigOrD a => a -> Tab -> SE a
readTable b1 b2 = fmap (fromGE . return) $ SE $ (depT =<<) $ lift $ f <$> toGE b1 <*> unTab b2
    where f a1 a2 = opcs "table" [(Ar,[Ar,Ir,Ir,Ir,Ir])
                                 ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                                 ,(Kr,[Kr,Ir,Ir,Ir,Ir])] [a1,a2]

-- | 
-- Notice that this function is the same as @tablei@, but it wraps the output in the SE-monad.
-- So you can use the @tablei@ if your table is read-only and you can use @readTablei@ if
-- you want to update the table and the order of read/write operation is important.
--
-- Accesses table values by direct indexing with cubic interpolation.
--
-- > ares  table3  andx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > ires  table3  indx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > kres  table3  kndx, ifn [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://www.csounds.com/manual/html/table3.html>
readTable3 :: SigOrD a => a -> Tab -> SE a
readTable3 b1 b2 = fmap (fromGE . return) $ SE $ (depT =<<) $ lift $ f <$> toGE b1 <*> unTab b2
    where f a1 a2 = opcs "table3" [(Ar,[Ar,Ir,Ir,Ir,Ir])
                                  ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                                  ,(Kr,[Kr,Ir,Ir,Ir,Ir])] [a1,a2]

-- | 
-- Notice that this function is the same as @table3@, but it wraps the output in the SE-monad.
-- So you can use the @table3@ if your table is read-only and you can use @readTable3@ if
-- you want to update the table and the order of read/write operation is important.
--
-- Accesses table values by direct indexing with linear interpolation.
--
-- > ares  tablei  andx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > ires  tablei  indx, ifn [, ixmode] [, ixoff] [, iwrap]
-- > kres  tablei  kndx, ifn [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://www.csounds.com/manual/html/tablei.html>
readTablei :: SigOrD a => a -> Tab -> SE a
readTablei b1 b2 = fmap (fromGE . return) $ SE $ (depT =<<) $ lift $ f <$> toGE b1 <*> unTab b2
    where f a1 a2 = opcs "tablei" [(Ar,[Ar,Ir,Ir,Ir,Ir])
                                  ,(Ir,[Ir,Ir,Ir,Ir,Ir])
                                  ,(Kr,[Kr,Ir,Ir,Ir,Ir])] [a1,a2]

-- | 
-- tableikt — Provides k-rate control over table numbers. 
--
-- k-rate control over table numbers. Function tables are read with linear interpolation. 
-- The standard Csound opcode tablei, when producing a k- or a-rate result, can only use an init-time variable to select the table number. tableikt accepts k-rate control as well as i-time. In all other respects they are similar to the original opcodes. 
--
-- > ares tableikt xndx, kfn [, ixmode] [, ixoff] [, iwrap]
-- > kres tableikt kndx, kfn [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://www.csounds.com/manual/html/tableikt.html>
tableikt ::  Sig -> Tab -> Sig
tableikt b1 b2 = Sig $ f <$> unSig b1 <*> unTab b2
    where f a1 a2 = opcs "tableikt" [(Ar,[Xr,Kr,Ir,Ir,Ir]),(Kr,[Xr,Kr,Ir,Ir,Ir])] [a1,a2]

-- | 
-- tablekt — Provides k-rate control over table numbers. 
--
-- k-rate control over table numbers. Function tables are read with linear interpolation. 
-- The standard Csound opcode table when producing a k- or a-rate result, can only use an init-time variable to select the table number. tablekt accepts k-rate control as well as i-time. In all other respects they are similar to the original opcodes. 
--
-- > ares tablekt xndx, kfn [, ixmode] [, ixoff] [, iwrap]
-- > kres tablekt kndx, kfn [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://www.csounds.com/manual/html/tablekt.html>
tablekt ::  Sig -> Tab -> Sig
tablekt b1 b2 = Sig $ f <$> unSig b1 <*> unTab b2
    where f a1 a2 = opcs "tablekt" [(Ar,[Xr,Kr,Ir,Ir,Ir]),(Kr,[Xr,Kr,Ir,Ir,Ir])] [a1,a2]


-- | 
-- tablexkt — Reads function tables with linear, cubic, or sinc interpolation. 
--
-- > ares tablexkt xndx, kfn, kwarp, iwsize [, ixmode] [, ixoff] [, iwrap]
--
-- csound doc: <http://www.csounds.com/manual/html/tablexkt.html>
tablexkt ::  Sig -> Tab -> Sig -> D -> Sig
tablexkt b1 b2 b3 b4 = Sig $ f <$> unSig b1 <*> unTab b2 <*> unSig b3 <*> unD b4
    where f a1 a2 a3 a4 = opcs "tablexkt" [(Ar,[Xr,Kr,Kr,Ir,Ir,Ir,Ir])] [a1,a2,a3,a4]

----------------------------------------------------------------
-- duserrnd and cuserrnd

-- | cuserrnd — Continuous USER-defined-distribution RaNDom generator.
--
-- Continuous USER-defined-distribution RaNDom generator.
--
-- > aout cuserrnd kmin, kmax, ktableNum
-- > iout cuserrnd imin, imax, itableNum
-- > kout cuserrnd kmin, kmax, ktableNum
--
-- csound doc: <http://www.csounds.com/manual/html/cuserrnd.html>
--
-- the tab should be done with tabDist, randDist or rangeDist
cuserrnd :: SigOrD a => a -> a -> Tab -> SE a
cuserrnd b1 b2 b3 = fmap (fromGE . return) $ SE $ (depT =<<) $ lift $ f <$> toGE b1 <*> toGE b2 <*> unTab b3
    where f a1 a2 a3 = opcs "cuserrnd" [(Ar,[Kr,Kr,Kr])
                                  ,(Ir,[Ir,Ir,Ir])
                                  ,(Kr,[Kr,Kr,Kr])] [a1,a2,a3] 

-- | duserrnd — Discrete USER-defined-distribution RaNDom generator.
--
-- Discrete USER-defined-distribution RaNDom generator.
--
-- > aout duserrnd ktableNum
-- > iout duserrnd itableNum
-- > kout duserrnd ktableNum
--
-- csound doc: <http://www.csounds.com/manual/html/duserrnd.html>
--
-- the tab should be done with tabDist, randDist or rangeDist
duserrnd :: SigOrD a => Tab -> SE a
duserrnd b1 = fmap (fromGE . return) $ SE $ (depT =<<) $ lift $ fmap f $ unTab b1
    where f a1 = opcs "duserrnd" [(Ar,[Kr])
                                  ,(Ir,[Ir])
                                  ,(Kr,[Kr])] [a1] 

----------------------------------------------------------------
-- tab args

relativeArgs :: [Double] -> TabArgs
relativeArgs xs = ArgsPlain $ reader $ \size -> fromRelative size xs
    where
        fromRelative n as = substEvens (mkRelative n $ getEvens as) as
          
        getEvens xs = case xs of
            [] -> []
            _:[] -> []
            _:b:as -> b : getEvens as
            
        substEvens evens xs = case (evens, xs) of
            ([], as) -> as
            (_, []) -> []
            (e:es, a:_:as) -> a : e : substEvens es as
            _ -> error "table argument list should contain even number of elements"
            
relativeArgsGen16 :: [Double] -> TabArgs
relativeArgsGen16 xs = ArgsPlain $ reader $ \size -> formRelativeGen16 size xs
    where            
        formRelativeGen16 n as = substGen16 (mkRelative n $ getGen16 as) as
         
          -- special case. subst relatives for Gen16
        formRelativeGen16 n as = substGen16 (mkRelative n $ getGen16 as) as

        getGen16 xs = case xs of
            _:durN:_:rest    -> durN : getGen16 rest
            _                -> []

        substGen16 durs xs = case (durs, xs) of 
            ([], as) -> as
            (_, [])  -> []
            (d:ds, valN:_:typeN:rest)   -> valN : d : typeN : substGen16 ds rest
            (_, _)   -> xs

mkRelative n as = fmap ((fromIntegral :: (Int -> Double)) . round . (s * )) as
    where s = fromIntegral n / sum as



