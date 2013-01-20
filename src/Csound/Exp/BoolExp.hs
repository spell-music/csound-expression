module Csound.Exp.BoolExp where

import Control.Applicative
import Data.Traversable
import Data.Foldable

import qualified Data.IntMap as IM
import Text.PrettyPrint

data CondInfo a = CondInfo
    { predCondInfo :: Cond
    , envCondInfo  :: IM.IntMap a
    } deriving (Show, Eq, Ord)    

data BoolExp a = BoolExp CondOp [a]
    deriving (Show, Eq, Ord)

data Cond 
    = CondPrim Int
    | CondExp CondOp [Cond]
    deriving (Show, Eq, Ord)    

data CondOp  
    = TrueOp | FalseOp
    | Not | And | Or
    | Equals | NotEquals | Less | Greater | LessEquals | GreaterEquals
    deriving (Show, Eq, Ord)    

-----------------------------------
-- instances for cse

instance Functor CondInfo where
    fmap f a = a{ envCondInfo = fmap f $ envCondInfo a }

instance Foldable CondInfo where
    foldMap f a = foldMap f $ envCondInfo a

instance Traversable CondInfo where
    traverse f a = CondInfo (predCondInfo a) <$> (traverse f $ envCondInfo a)

instance Functor BoolExp where
    fmap f (BoolExp op as) = BoolExp op $ fmap f as

instance Foldable BoolExp where
    foldMap f (BoolExp  _ as) = foldMap f as

instance Traversable BoolExp where
    traverse f (BoolExp op as) = BoolExp op <$> traverse f as

-----------------------------------

isTrue, isFalse :: CondInfo a -> Bool

isTrue  = isCondOp TrueOp
isFalse = isCondOp FalseOp

isCondOp op = maybe False (op == ) . getCondInfoOp

getCondInfoOp :: CondInfo a -> Maybe CondOp
getCondInfoOp x = case predCondInfo x of
    CondExp op _ -> Just op
    _ -> Nothing


negateCondOp :: CondOp -> CondOp
negateCondOp x = case x of
    TrueOp            -> FalseOp                
    FalseOp           -> TrueOp
    Not               -> error "can not negate Not" 
    And               -> Or
    Or                -> And
    Equals            -> NotEquals
    NotEquals         -> Equals
    Less              -> GreaterEquals
    Greater           -> LessEquals
    LessEquals        -> Greater
    GreaterEquals     -> Less    

------------------------------------
-- render

renderCondInfo :: (a -> Doc) -> CondInfo a -> Doc
renderCondInfo renderLeaf a = renderCond $ predCondInfo a    
    where leaf n = renderLeaf $ envCondInfo a IM.! n
          renderCond x = case x of
              CondPrim n        -> leaf n
              CondExp op args   -> renderOp op $ map renderCond args    

          renderOp :: CondOp -> [Doc] -> Doc  
          renderOp op args = case op of
              TrueOp            -> text "(1 == 1)"                
              FalseOp           -> text "(0 == 1)"
              Not               -> uno "~" 
              And               -> bi "&&"
              Or                -> bi "||"
              Equals            -> bi "=="
              NotEquals         -> bi "!="
              Less              -> bi "<"
              Greater           -> bi ">"
              LessEquals        -> bi "<="    
              GreaterEquals     -> bi ">="                         
              where bi  op = parens $ args !! 0 <+> text op <+> args !! 1
                    uno op = parens $ text op <> args !! 0
          



