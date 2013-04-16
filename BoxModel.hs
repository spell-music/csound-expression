module Box where

import Control.Monad.Trans.State.Strict
import Data.Default


data Interval = Interval 
    { start :: Int
    , leng  :: Int 
    } deriving (Show)
    
data Rect = Rect 
    { px        :: Int
    , py        :: Int
    , width     :: Int
    , height    :: Int
    } deriving (Show)

fromRect r = (Interval (px r) (width r), Interval (py r) (height r))
toRect a b = Rect (start a) (start b) (leng a) (leng b)
          
data Scene 
    = Prim 
    | Space   
    | Scale Double Scene
    | Hor Offset [Scene]
    | Ver Offset [Scene]
    deriving (Show)

data Offset = Offset 
    { offsetMargin    :: Int
    , offsetPadding   :: Int 
    } deriving (Show)

instance Default Offset where
    def = Offset 3 3

hor, ver :: [Scene] -> Scene
space, prim :: Scene

sca :: Double -> Scene -> Scene
margin  :: Int -> Scene -> Scene
padding :: Int -> Scene -> Scene

hor     = Hor def
ver     = Ver def
sca     = Scale
space   = Space
prim    = Prim

margin  n = withOffset (\x -> x{ offsetMargin  = n })
padding n = withOffset (\x -> x{ offsetPadding = n })

withOffset f x = case x of
    Hor off as -> Hor (f off) as
    Ver off as -> Ver (f off) as
    _ -> x

draw :: Rect -> Scene -> [Rect]
draw rect x = case x of
    Space -> []
    Prim  -> [rect]
    Scale d a -> draw rect a
    Hor off as -> composite (horRects rect) off as
    Ver off as -> composite (verRects rect) off as

composite getRects off as = concat $ zipWith draw (getRects off $ factors as) (fmap stripScale as)
   
horRects rect off scales = fmap (flip toRect commonSide) is 
    where commonSide = withoutMargin off iy
          is = intervals off ix scales  
          (ix, iy) = fromRect rect

verRects rect off scales = fmap (toRect commonSide) is 
    where commonSide = withoutMargin off ix
          is = intervals off iy scales  
          (ix, iy) = fromRect rect  

intervals :: Offset -> Interval -> [Double] -> [Interval]
intervals off total scales = evalState (mapM next scales') (start total') 
    where total'  = withoutMargin off total
          leng'   = withoutPaddings off (length scales) (leng total')
          scales' = fmap ( / s) scales
          s       = sum scales
          totalLeng = fromIntegral $ leng total'  

          next d  = state $ \soFar -> let l = round $ d * totalLeng
                                      in  (Interval soFar l, soFar + l + offsetPadding off)
            
withoutPaddings off n a = a - offsetPadding off * (n - 1)

withoutMargin :: Offset -> Interval -> Interval
withoutMargin off a = Interval (start a + offsetMargin off) (leng a - 2 * offsetMargin off)

scaleRect :: Double -> Rect -> Rect
scaleRect d (Rect x y w h)  = Rect (f x) (f y) (f w) (f h)
    where f a = round $ d * fromIntegral a

factors :: [Scene] -> [Double]
factors = fmap factor
    where factor = maybe 1 fst . maybeScale

stripScale :: Scene -> Scene
stripScale x = maybe x snd $ maybeScale x

maybeScale :: Scene -> Maybe (Double, Scene)
maybeScale x = case x of
    Scale d x   -> Just (d, x)    
    _           -> Nothing

