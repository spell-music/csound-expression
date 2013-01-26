module Csound.Exp.Inline where

import Control.Applicative
import Data.Traversable
import Data.Foldable

import qualified Data.IntMap as IM
import Text.PrettyPrint

data Inline a b = Inline 
    { inlineExp :: InlineExp a
    , inlineEnv :: IM.IntMap b    
    } deriving (Show, Eq, Ord)

data InlineExp a
    = InlinePrim Int
    | InlineExp a [InlineExp a]
    deriving (Show, Eq, Ord)

data PreInline a b = PreInline a [b]
    deriving (Show, Eq, Ord)

-----------------------------------------
-- rendering

renderInline :: (a -> [Doc] -> Doc) -> (b -> Doc) -> Inline a b -> Doc
renderInline renderNode renderLeaf a = renderExp $ inlineExp a    
    where leaf n = renderLeaf $ inlineEnv a IM.! n
          renderExp x = case x of
              InlinePrim n        -> leaf n
              InlineExp op args   -> renderNode op $ map renderExp args  

-----------------------------------------
-- instances for cse

instance Functor (Inline a) where
    fmap f a = a{ inlineEnv = fmap f $ inlineEnv a }

instance Foldable (Inline a) where
    foldMap f a = foldMap f $ inlineEnv a

instance Traversable (Inline a) where
    traverse f (Inline a b) = Inline a <$> traverse f b

instance Functor (PreInline a) where
    fmap f (PreInline op as) = PreInline op $ fmap f as

instance Foldable (PreInline a) where
    foldMap f (PreInline _ as) = foldMap f as

instance Traversable (PreInline a) where
    traverse f (PreInline op as) = PreInline op <$> traverse f as

