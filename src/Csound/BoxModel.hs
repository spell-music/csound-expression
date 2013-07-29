{-# Language DeriveFunctor #-}
module Csound.BoxModel(
    Rect(..), Offset(..), AbsScene(..), Scene(..),        
    draw,
    hor, ver, sca, margin, padding, space, prim,
    appendContext, cascade
) where

import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Monoid

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

fromRect :: Rect -> (Interval, Interval)
fromRect r = (Interval (px r) (width r), Interval (py r) (height r))

toRect :: Interval -> Interval -> Rect
toRect a b = Rect (start a) (start b) (leng a) (leng b)
  
data AbsScene ctx a 
    = Elem Rect a
    | EmptyScene 
    | Group [AbsScene ctx a]
    | Ctx ctx (AbsScene ctx a)
    deriving (Show)
     
instance Monoid (AbsScene ctx a) where
    mempty = EmptyScene
    mappend a b = case (a, b) of
        (EmptyScene, _) -> b
        (_, EmptyScene) -> a        
        (Elem _ _, Group bs) -> Group (a:bs)
        (Group as, Elem _ _) -> Group (as ++ [b])
        (Group as, Group bs)   -> Group (as ++ bs)
        (_, _) -> Group [a, b]
   
data Scene ctx a
    = Prim a
    | Space   
    | Scale Double (Scene ctx a)
    | Hor Offset [Scene ctx a]
    | Ver Offset [Scene ctx a]
    | Context ctx (Scene ctx a)
    deriving (Show, Functor)

instance Monad (Scene ctx) where
    return = Prim
    ma >>= mf = joinScene $ fmap mf ma
        where
            joinScene :: Scene ctx (Scene ctx a) -> Scene ctx a
            joinScene x = case x of
                Prim rec    -> rec
                Space       -> Space
                Scale   d a -> Scale   d (joinScene a)
                Hor     o a -> Hor     o (fmap joinScene a)
                Ver     o a -> Ver     o (fmap joinScene a)
                Context c a -> Context c (joinScene a)

data Offset = Offset 
    { offsetOuter :: Int
    , offsetInner :: Int 
    } deriving (Show)

instance Default Offset where
    def = Offset 20 20

appendContext :: Monoid ctx => ctx -> Scene ctx a -> Scene ctx a
appendContext ctx x = case x of
    Context oldCtx a    -> Context (mappend ctx oldCtx) a
    _                   -> Context ctx x

hor, ver    :: [Scene a b] -> Scene a b
space       :: Scene a b
prim        :: a -> Scene ctx a 

sca :: Double -> Scene a b -> Scene a b
margin, padding :: Int -> Scene a b -> Scene a b

hor     = Hor def
ver     = Ver def
sca     = Scale
space   = Space
prim    = Prim

margin  n = withOffset (\x -> x{ offsetOuter = n })
padding n = withOffset (\x -> x{ offsetInner = n })

withOffset :: (Offset -> Offset) -> Scene ctx a -> Scene ctx a
withOffset f x = case x of
    Hor off as -> Hor (f off) as
    Ver off as -> Ver (f off) as
    _ -> x

draw :: Rect -> Scene ctx a -> AbsScene ctx a
draw rect x = case x of
    Space  -> mempty
    Prim a -> Elem rect a
    Scale d a -> draw (scaleRect d rect) a
    Hor off as -> composite (horRects rect) off as
    Ver off as -> composite (verRects rect) off as
    Context ctx a -> Ctx ctx (draw rect a)
    where 
        composite getRects off as = mconcat $ zipWith draw (getRects off $ factors as) (fmap stripScale as)
   
        horRects r off scales = fmap (flip toRect commonSide) is 
            where commonSide = withoutMargin off iy
                  is = intervals off ix scales  
                  (ix, iy) = fromRect r

        verRects r off scales = fmap (toRect commonSide) is 
            where commonSide = withoutMargin off ix
                  is = intervals off iy scales  
                  (ix, iy) = fromRect r  

intervals :: Offset -> Interval -> [Double] -> [Interval]
intervals off total scales = evalState (mapM next scales') (start total') 
    where total'  = withoutMargin off total
          leng'   = fromIntegral $ withoutPaddings off (length scales) (leng total')
          scales' = fmap ( / s) scales
          s       = sum scales

          next d  = state $ \soFar -> let l = round $ d * leng'
                                      in  (Interval soFar l, soFar + l + offsetInner off)
            
          withoutPaddings offset n a = a - offsetInner offset * (n - 1)

withoutMargin :: Offset -> Interval -> Interval
withoutMargin off a = Interval (start a + offsetOuter off) (leng a - 2 * offsetOuter off)

scaleRect :: Double -> Rect -> Rect
scaleRect d (Rect x y w h)  = Rect (f x) (f y) (f w) (f h)
    where f a = round $ d * fromIntegral a

factors :: [Scene a b] -> [Double]
factors = fmap factor
    where factor = maybe 1 fst . maybeScale

stripScale :: Scene a b -> Scene a b
stripScale x = maybe x snd $ maybeScale x

maybeScale :: Scene a b -> Maybe (Double, Scene a b)
maybeScale x = case x of
    Scale d a   -> Just (d, a)
    _           -> Nothing

-----------------------------------------------
-- cascading update of the context

cascade :: 
       (totalCtx -> Rect -> a -> res) 
    -> res 
    -> ([res] -> res)
    -> (ctx -> totalCtx -> totalCtx)
    -> totalCtx -> AbsScene ctx a -> res
cascade onElem onEmptyScene onGroup onCtx ctx x = case x of
    Elem r a    -> onElem ctx r a
    EmptyScene  -> onEmptyScene
    Group as    -> onGroup (fmap (rec ctx) as)
    Ctx c a     -> rec (onCtx c ctx) a
    where rec = cascade onElem onEmptyScene onGroup onCtx

