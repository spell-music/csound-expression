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
    consts, lins, cubes, exps, splines,    
    -- ** Equally spaced interpolants
    econsts, elins, ecubes, eexps, esplines,

    -- * Polynomials    
    polys, chebs1, chebs2, bessels,
    
    -- * Windows
    wins, WinType(..), 

    -- * Low level Csound definition.
    gen,
    
    -- * Modify tables
    skipNorm, setSize, setDegree, guardPoint, gp,
    
    -- ** Handy shortcuts        
    -- | handy shortcuts for the function 'setDegree'.
    lllofi, llofi, lofi, midfi, hifi, hhifi, hhhifi,
    
    -- * Identifiers for GEN-routines
    
    -- | Low level Csound integer identifiers for tables. These names can be used in the function 'Csound.Base.fineFi'
    idWavs, idMp3s, idDoubles, idSines, idSines3, idSines2, idPartials, idSines4, idBuzzes, idConsts, idLins, idCubes, idExps, idSplines,  idPolys, idChebs1, idChebs2, idBessels, idWins
) where

import Data.Maybe(listToMaybe)
import Data.Default
import Csound.Exp
import Csound.Tfm.Tab(updateTabSize)

import qualified Data.IntMap as IM


wavs :: String -> Double -> Int -> Tab
wavs filename skiptime channel = Tab (SizePlain 0) idWavs 
    (FileAccess filename [skiptime, format, fromIntegral $ channel])
    where format = 0

mp3s :: String -> Double -> Tab
mp3s filename skiptime = Tab (SizePlain 0) idMp3s 
    (FileAccess filename [skiptime, format])
    where format = 0

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
fineFi :: Int -> [(Int, Int)] -> TabFi
fineFi n xs = TabFi n (IM.fromList xs)

-- | Sets the same table size for all tables. 
--
-- > coarseFi n
--
-- where @n@  is a degree of 2. For example, @n = 10@ sets size to 1024 points for all tables by default.
coarseFi :: Int -> TabFi
coarseFi n = TabFi n IM.empty

interp :: Int -> [Double] -> Tab
interp genId as = Tab def genId (ArgsRelative as)

plains :: Int -> [Double] -> Tab
plains genId as = Tab def genId (ArgsPlain as)

insertOnes :: [Double] -> [Double]
insertOnes xs = case xs of
    [] -> []
    a:[] -> [a]
    a:as -> a : 1 : insertOnes as


tableSizes :: [Int]
tableSizes = [res | a <- twos, b <- twos1, res <- [a, b]]
    where twos  = fmap (2 ^) [(0::Int) .. ]
          twos1 = fmap ( +1) twos  

findTableSize :: Int -> Int
findTableSize n = head $ dropWhile (< n) tableSizes

-- loadFile :: Int -> String -> Double -> Tab

-- | Table contains all provided values 
-- (table is extended to contain all values and to be of the power of 2 or the power of two plus one).
doubles :: [Double] -> Tab
doubles as = setSize (findTableSize n) $ plains idDoubles as
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
sines2 xs = sines3 [(num, str, 0) | (num, str) <- xs]

-- | Specifies series of possibly inharmonic partials.
sines3 :: [(PartialNumber, PartialStrength, PartialPhase)] -> Tab
sines3 xs = plains idSines3 [a | (pn, str, phs) <- xs, a <- [pn, str, phs]]

-- | Specifies series of possibly inharmonic partials with direct current.
sines4 :: [(PartialNumber, PartialStrength, PartialPhase, PartialDC)] -> Tab
sines4 xs = plains idSines4 [a | (pn, str, phs, dc) <- xs, a <- [pn, str, phs, dc]]

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

data WinType 
    = Hamming | Hanning | Bartlett | Blackman
    | Harris | Gaussian Double | Kaiser Double
    | RectWin | SyncWin

winTypeId :: WinType -> Double
winTypeId x = case x of
    Hamming     -> 1
    Hanning     -> 2
    Bartlett    -> 3
    Blackman    -> 4
    Harris      -> 5
    Gaussian _  -> 6
    Kaiser _    -> 7
    RectWin     -> 8
    SyncWin     -> 9

winOptParam :: WinType -> Maybe Double
winOptParam x = case x of
    Gaussian a  -> Just a
    Kaiser a    -> Just a
    _           -> Nothing

winMaxParam :: [Double] -> Maybe Double
winMaxParam = listToMaybe 

wins :: WinType -> [Double] -> Tab
wins ty maxs = gen idWins (winTypeId ty : params)
    where params = case (winMaxParam maxs, winOptParam ty) of
            (Nothing, Nothing) -> []
            (Nothing, Just x)  -> [1, x]
            (Just x,  Nothing) -> [x]
            (Just x, Just y)   -> [x, y]

-- | Creates a table of doubles (It's f-table in Csound).
-- Arguments are:
--
-- * identificator of the GEN routine
--
-- * GEN routine arguments
--
-- All tables are created at 0 and memory is never released.
gen :: Int -> [Double] -> Tab
gen genId args = Tab def genId (ArgsPlain args)

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

-- | Skips normalization (sets table size to negative value)
skipNorm :: Tab -> Tab
skipNorm x = case x of
    TabExp _ -> error "you can skip normalization only for primitive tables (made with gen-routines)"
    primTab  -> primTab{ tabGen = negate $ abs $ tabGen primTab }



idWavs, idMp3s, idDoubles, idSines, idSines3, idSines2, idPartials, idSines4, idBuzzes, idConsts, idLins, idCubes, idExps, idSplines,  idPolys, idChebs1, idChebs2, idBessels, idWins :: Int

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
idSplines = 8 
idPolys = 3
idChebs1 = 13
idChebs2 = 14
idBessels = 12
idWins = 20

idMp3s = 49
