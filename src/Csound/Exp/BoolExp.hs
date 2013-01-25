module Csound.Exp.BoolExp where

import Control.Applicative
import Data.Traversable
import Data.Foldable

import qualified Data.IntMap as IM
import Text.PrettyPrint

import Csound.Exp.Inline

type BoolExp a = PreInline CondOp a
type CondInfo a = Inline CondOp a

data CondOp  
    = TrueOp | FalseOp
    | Not | And | Or
    | Equals | NotEquals | Less | Greater | LessEquals | GreaterEquals
    deriving (Show, Eq, Ord)    

-----------------------------------

isTrue, isFalse :: CondInfo a -> Bool

isTrue  = isCondOp TrueOp
isFalse = isCondOp FalseOp

isCondOp op = maybe False (op == ) . getCondInfoOp

getCondInfoOp :: CondInfo a -> Maybe CondOp
getCondInfoOp x = case inlineExp x of
    InlineExp op _ -> Just op
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
renderCondInfo = renderInline renderOp 

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
          



