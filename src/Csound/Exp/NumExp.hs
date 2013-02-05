module Csound.Exp.NumExp(
    NumExp, NumOp(..), renderNumExp,
    NumOpt(..),
    add, sub, mul, div, neg,
    exp', sin', cos', sinh', cosh', tan', tanh', sininv', cosinv', taninv',
    abs', ceil', floor', frac', int', log', log10', logbtwo', round', sqrt',
    pow'
) where

import Prelude hiding (div)

import Control.Applicative

import Data.Maybe(fromJust)

import Text.PrettyPrint

import Csound.Exp.Inline
import Csound.Render.PrettyOp

type NumExp a = PreInline NumOp a

data NumOp 
    = Add | Sub | Neg | Mul | Div
    | Pow | Mod 
    | Sin | Cos | Sinh | Cosh | Tan | Tanh | Sininv | Cosinv | Taninv
    | Abs | Ceil | ExpOp | Floor | Frac| IntOp | Log | Log10 | Logbtwo | Round | Sqrt    
    | Ampdb | Ampdbfs | Dbamp | Dbfsamp 
    | Cpspch
    deriving (Show, Eq, Ord)

renderNumExp leaf (PreInline op as) = renderNumOp op $ fmap leaf as

renderNumExp :: (a -> Doc) -> NumExp a -> Doc

renderNumOp :: NumOp -> [Doc] -> Doc
renderNumOp op = case  op of
    Add -> bi "+"
    Sub -> bi "-"
    Mul -> bi "*"
    Div -> bi "/"
    Neg -> uno "-"    
    Pow -> bi "^"
    Mod -> bi "%"
    Sin -> fun "sin"
    Cos -> fun "cos"
    Sinh -> fun "sinh"    
    Cosh -> fun "cosh"
    Tan -> fun "tan"
    Tanh -> fun "tanh"    
    Sininv -> fun "sininv"
    Cosinv -> fun "cosinv"
    Taninv -> fun "taninv"        
    Abs -> fun "abs"    
    Ceil -> fun "ceil" 
    ExpOp -> fun "exp"
    Floor -> fun "floor" 
    Frac -> fun "frac"
    IntOp -> fun "int" 
    Log -> fun "log" 
    Log10 -> fun "log10" 
    Logbtwo -> fun "logbtwo" 
    Round -> fun "round"
    Sqrt -> fun "sqrt"    

    Ampdb -> fun "ampdb" 
    Ampdbfs -> fun "ampdbfs" 
    Dbamp -> fun "dbamp"
    Dbfsamp -> fun "dbfsamp"    
    Cpspch -> fun "cpspch"
    where bi  = binaries
          uno = unaries
          fun = funcs
            

class NumOpt a where
    maybeInt    :: a -> Maybe Int
    maybeDouble :: a -> Maybe Double
    fromInt     :: Int -> a
    fromDouble  :: Double -> a
    fromNum     :: NumExp a -> a
    
isZero :: NumOpt a => a -> Bool
isZero a = maybe False id $ liftA2 (||) ((== 0) <$> maybeInt a) ((==0) <$> maybeDouble a)

getDouble :: NumOpt a => a -> Maybe Double
getDouble a = maybe (fromIntegral <$> maybeInt a) Just $ maybeDouble a 

unOpt :: (NumOpt a) => (Int -> Int) -> (Double -> Double) -> NumOp -> a -> a
unOpt intOp doubleOp op a = fromJust $
        (fromInt . intOp <$> maybeInt a)
    <|> (fromDouble . doubleOp <$> maybeDouble a)
    <|> Just (noOpt1 op a)

biOpt :: (NumOpt a) => (Int -> Int -> Int) -> (Double -> Double -> Double) -> NumOp -> a -> a -> a
biOpt intOp doubleOp op a b = fromJust $
        intOpA (maybeInt a) (maybeInt b) 
    <|> doubleOpA (getDouble a) (getDouble b)
    <|> Just (noOpt2 op a b)
    where intOpA a b = fromInt <$> liftA2 intOp a b
          doubleOpA a b = fromDouble <$> liftA2 doubleOp a b

biOptOnDouble :: (NumOpt a) => (Double -> Double -> Double) -> NumOp -> a -> a -> a
biOptOnDouble doubleOp op a b = fromJust $
        (fromDouble <$> liftA2 doubleOp (getDouble a) (getDouble b))
    <|> Just (noOpt2 op a b) 
        

funOpt :: NumOpt a => (Double -> Double) -> NumOp -> a -> a
funOpt doubleOp op a = fromJust $
        (fromDouble . doubleOp <$> getDouble a)
    <|> Just (noOpt1 op a)

noOpt1 :: NumOpt a => NumOp -> a -> a
noOpt1 op a = fromNum $ PreInline op [a]

noOpt2 :: NumOpt a => NumOp -> a -> a -> a
noOpt2 op a b = fromNum $ PreInline op [a, b]

doubleToInt :: NumOpt a => (Double -> Int) -> NumOp -> a -> a
doubleToInt fun op a = fromJust $
        (fromInt <$> maybeInt a)
    <|> (fromInt . fun <$> maybeDouble a)
    <|> Just (noOpt1 op a)

-- arithmetic

add, sub, mul, div :: NumOpt a => a -> a -> a
neg :: NumOpt a => a -> a

add a b 
    | isZero a = b
    | isZero b = a
    | otherwise = biOpt (+) (+) Add a b
    
mul a b 
    | isZero a || isZero b = fromInt 0
    | otherwise = biOpt (*) (*) Mul a b

sub a b  
    | isZero a = neg b
    | isZero b = a
    | otherwise = biOpt (-) (-) Sub a b   
   
div a b 
    | isZero a = fromInt 0
    | isZero b = error "csound (/): division by zero" 
    | otherwise = biOptOnDouble (/) Div a b
 
neg = unOpt negate negate Neg
 
pow' :: NumOpt a => a -> a -> a
pow' = biOpt (^) (**) Pow

mod' :: NumOpt a => a -> a -> a
mod' = biOpt mod (\a b -> fromIntegral $ mod (floor a) (floor b)) Pow
 
-- trigonometry
    
exp', sin', cos', sinh', cosh', tan', tanh', sininv', cosinv', taninv' :: NumOpt a => a -> a 

exp'    = funOpt exp ExpOp
sin'    = funOpt sin Sin 
cos'    = funOpt cos Cos
sinh'   = funOpt sinh Sinh
cosh'   = funOpt cosh Cosh
tan'    = funOpt tan Tan
tanh'   = funOpt tanh Tanh
sininv' = funOpt asin Sininv
cosinv' = funOpt acos Cosinv
taninv' = funOpt atan Taninv

-- other functions

abs', ceil', floor', frac', int', log', log10', logbtwo', round', sqrt' :: NumOpt a => a -> a
    
abs'    = unOpt abs abs Abs
ceil'   = doubleToInt ceiling Ceil 
floor'  = doubleToInt floor Floor
round'  = doubleToInt round Round
frac'   = unOpt (const 0) (snd . properFraction) Frac 
int'    = doubleToInt truncate IntOp 
log'    = funOpt log Log
log10'  = funOpt (flip logBase 10) Log10
logbtwo' = funOpt (flip logBase 2) Logbtwo 
sqrt'   = funOpt sqrt Sqrt
    



    


