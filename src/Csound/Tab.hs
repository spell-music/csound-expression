-- | Creating Function Tables (Buffers)
module Csound.Tab (
    -- | If you are not familliar with Csound's conventions
    -- you are pobably not aware of the fact that for efficiency reasons Csound requires that table size is equal
    -- to power of 2 or power of two plus one which stands for guard point (you do need guard point if your intention is to read the 
    -- table once but you don't need the guard point if you read the table in many cycles, then the guard point is the the first point of your table).  
    Tab,

    -- * Table granularity
    TabFi, fineFi, coarseFi,        

    -- * Fill table with numbers
    doubles,
   
    -- * Read from files
    wavs, mp3s,

    -- * (In)Harmonic series
    PartialStrength, PartialNumber, PartialPhase, PartialDC,
    sines, sines3, sines2, partials, sines4, buzzes,
    -- ** Special cases
    sine, cosine, sigmoid,

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
    consts, lins, cubes, exps, splines, startEnds,
    -- ** Equally spaced interpolants
    econsts, elins, ecubes, eexps, esplines, estartEnds,

    -- * Polynomials    
    polys, chebs1, chebs2, bessels,
    
    -- * Windows  
    winHamming, winHanning,  winBartlett, winBlackman,
    winHarris, winGaussian, winKaiser, winRectangle, winSync,

    -- * Low level Csound definition.
    gen,
    
    -- * Modify tables
    skipNorm, forceNorm, setSize, setDegree, guardPoint, gp,
    
    -- ** Handy shortcuts        
    -- | handy shortcuts for the function 'setDegree'.
    lllofi, llofi, lofi, midfi, hifi, hhifi, hhhifi,
    
    -- * Identifiers for GEN-routines
    
    -- | Low level Csound integer identifiers for tables. These names can be used in the function 'Csound.Base.fineFi'
    idWavs, idMp3s, idDoubles, idSines, idSines3, idSines2
    , idPartials, idSines4, idBuzzes, idConsts, idLins, idCubes
    , idExps, idSplines, idStartEnds,  idPolys, idChebs1, idChebs2, idBessels, idWins
) where

import Data.Default
import Csound.Typed

wavs :: String -> Double -> Int -> Tab
wavs filename skiptime channel = preTab (SizePlain 0) idWavs 
    (FileAccess filename [skiptime, format, fromIntegral $ channel])
    where format = 0

mp3s :: String -> Double -> Tab
mp3s filename skiptime = preTab (SizePlain 0) idMp3s 
    (FileAccess filename [skiptime, format])
    where format = 0

interp :: Int -> [Double] -> Tab
interp genId as = preTab def genId (ArgsRelative as)

plains :: Int -> [Double] -> Tab
plains genId as = preTab def genId (ArgsPlain as)

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
startEnds as = preTab def idStartEnds (ArgsGen16 as)

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
partials :: [PartialNumber] -> Tab
partials xs = sines2 $ zip xs (repeat 1)

-- | Just like 'Csound.Tab.sines3' but phases are set to zero.
sines2 :: [(PartialNumber, PartialStrength)] -> Tab
sines2 xs = sines3 [(num, strength, 0) | (num, strength) <- xs]

-- | Specifies series of possibly inharmonic partials.
sines3 :: [(PartialNumber, PartialStrength, PartialPhase)] -> Tab
sines3 xs = plains idSines3 [a | (pn, strength, phs) <- xs, a <- [pn, strength, phs]]

-- | Specifies series of possibly inharmonic partials with direct current.
sines4 :: [(PartialNumber, PartialStrength, PartialPhase, PartialDC)] -> Tab
sines4 xs = plains idSines4 [a | (pn, strength, phs, dc) <- xs, a <- [pn, strength, phs, dc]]

-- | Table for pure sine wave.
sine :: Tab
sine = sines [1]

-- | Table for pure cosine wave.
cosine :: Tab
cosine = buzzes 1 []

-- | Table for sigmoid wave.
sigmoid :: Tab
sigmoid = sines4 [(0.5, 0.5, 270, 0.5)]

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

-- | Creates a table of doubles (It's f-table in Csound).
-- Arguments are:
--
-- * identificator of the GEN routine
--
-- * GEN routine arguments
--
-- All tables are created at 0 and memory is never released.
gen :: Int -> [Double] -> Tab
gen genId args = preTab def genId (ArgsPlain args)

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

