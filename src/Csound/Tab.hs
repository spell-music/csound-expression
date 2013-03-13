-- | Creating Function Tables (Buffers)
module Csound.Tab (
    -- | If you are not familliar with Csound's conventions
    -- you are pobably not aware of the fact that for efficiency reasons Csound requires that table size is equal
    -- to power of 2 or power of two plus one which stands for guard point (you do need guard point if your intention is to read the 
    -- table once but you don't need the guard point if you read the table in many cycles, then the guard point is the the first point of your table).  
    Tab,
    
    -- * Fill table with numbers
    doubles,
    
    -- * (In)Harmonic series
    PartialStrength, PartialNumber, PartialPhase, PartialDC,
    sines, sines3, sines4, buzzes,

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
    -- > segs [0, 0.25, 1, 0.75, 0] 
    --
    -- or
    --
    -- > segs [0, 25, 1, 75, 0]
    --
    -- or
    --
    -- > segs [0, 1, 1, 3, 0]
    --
    -- all these expressions are equivalent. 
    consts, segs, cubes, exps, splines,    
    -- ** Equally spaced interpolants
    econsts, esegs, ecubes, eexps, esplines,

    -- * Polynomials    
    polys, chebs1, chebs2,
    
    -- 

    -- * Low level Csound definition.
    gen,
    
    -- * Modify tables
    skipNorm, setSize, setDegree,
    
    -- ** Handy shortcuts        
    -- | handy shortcuts for the function 'setDegree'.
    lllofi, llofi, lofi, midfi, hifi, hhifi, hhhifi
) where

import Data.Default
import Csound.Exp
import Csound.Exp.Wrapper(updateTabSize)


interp id as = Tab def id (ArgsRelative as)
plains id as = Tab def id (ArgsPlain as)


tableSizes :: [Int]
tableSizes = [res | a <- twos, b <- twos1, res <- [a, b]]
    where twos  = fmap (2 ^) [0 .. ]
          twos1 = fmap ( +1) twos  

findTableSize :: Int -> Int
findTableSize n = head $ dropWhile (< n) tableSizes

-- loadFile :: Int -> String -> Double -> Tab

-- | Table contains all provided values 
-- (table is extended to contain all values and to be of the power of 2 or the power of two plus one).
doubles :: [Double] -> Tab
doubles as = setSize (findTableSize n) $ plains 2 as
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
exps = interp 5

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
cubes = interp 6

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
-- > segs [a, n1, b, n2, c, ...]
--
-- where
--
-- * a, b, c .. - are ordinate values
--
-- * @n1, n2, ...@  are lengths of the segments relative to the total number of the points in the table
segs :: [Double] -> Tab
segs = interp 7

-- | Equally spaced segments of straight lines.
--
-- > esegs [a, b, c, ...] 
--
-- is the same as
--
-- > segs [a, 1, b, 1, c, ...]
esegs :: [Double] -> Tab
esegs = esegs . insertOnes

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
splines = interp 8

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
consts = interp 17

-- | Equally spaced constant segments.
--
-- > econsts [a, b, c, ...] 
--
-- is the same as
--
-- > consts [a, 1, b, 1, c, ...]
econsts :: [Double] -> Tab
econsts = consts . insertOnes

insertOnes :: [Double] -> [Double]
insertOnes as = case as of
    [] -> []
    a:[] -> [a]
    a:as -> a : 1 : insertOnes as
    
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
sines = plains 10

-- | Specifies series of possibly inharmonic partials.
sines3 :: [(PartialNumber, PartialStrength, PartialPhase)] -> Tab
sines3 xs = plains 9 [a | (pn, str, phs) <- xs, a <- [pn, str, phs]]

-- | Specifies series of possibly inharmonic partials with direct current.
sines4 :: [(PartialNumber, PartialStrength, PartialPhase, PartialDC)] -> Tab
sines4 xs = plains 19 [a | (pn, str, phs, dc) <- xs, a <- [pn, str, phs, dc]]

-- | Generates values similar to the opcode @buzz@. 
--
-- > buzzes numberOfHarmonics [lowestHarmonic, coefficientOfAttenuation]
--
-- With @buzzes n [l, r]@ you get @n@ harmonics from @l@ that are attenuated by the factor of @r@
-- on each step.
buzzes :: Double -> [Double] -> Tab
buzzes nh opts = plains 11 (nh : take 2 opts)

-- | Modified Bessel function of the second kind, order 0 (for amplitude modulated FM). 
--
-- > bessels xint
--
-- the function is defined within the interval @[0, xint]@.
bessels :: Double -> Tab
bessels xint = plains 12 [xint]

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
polys x0 x1 cs = plains 3 (x0:x1:cs)

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
chebs1 xint xamp hs = plains 13 (xint : xamp : hs)

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
chebs2 xint xamp hs = plains 14 (xint : xamp : hs)


-- | Creates a table of doubles (It's f-table in Csound).
-- Arguments are:
--
-- * identificator of the GEN routine
--
-- * GEN routine arguments
--
-- All tables are created at 0 and memory is never released.
gen :: Int -> [Double] -> Tab
gen id args = Tab def id (ArgsPlain args)

-- | Adds guard point to the table size (details of the interpolation schemes: you do need guard point if your intention is to read the 
-- table once but you don't need the guard point if you read table in many cycles, the guard point is the the first point of your table).  
guardPoint :: Tab -> Tab
guardPoint = updateTabSize $ \x -> case x of
    SizePlain n -> SizePlain (n + 1)
    a -> a{ hasGuardPoint = True }    

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
hifi    = setDegree 2
hhifi   = setDegree 1
hhhifi  = setDegree 3 

-- | Skips normalization (sets table size to negative value)
skipNorm :: Tab -> Tab
skipNorm = updateTabSize phi
    where phi size = case size of 
            SizePlain n -> SizePlain $ negate $ abs n
            x -> x{ isNormalized = False }




