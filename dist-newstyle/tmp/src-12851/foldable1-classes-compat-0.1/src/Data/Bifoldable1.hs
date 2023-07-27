{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}

#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe              #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy       #-}
#endif
module Data.Bifoldable1 where

import Control.Applicative (Const (..))
import Data.Bifoldable     (Bifoldable (..))
import Data.Semigroup      (Arg (..), Semigroup (..))
import Prelude             (Either (..), id)

#ifdef MIN_VERSION_tagged
import Data.Tagged (Tagged (..))
#endif

#if !MIN_VERSION_base(4,12,0)
import Data.Orphans ()
#endif

class Bifoldable t => Bifoldable1 t where
     bifold1 :: Semigroup m => t m m -> m
     bifold1 = bifoldMap1 id id
     {-# INLINE bifold1 #-}

     bifoldMap1 :: Semigroup m => (a -> m) -> (b -> m) -> t a b -> m

instance Bifoldable1 Arg where
    bifoldMap1 f g (Arg a b) = f a <> g b

instance Bifoldable1 Either where
    bifoldMap1 f _ (Left a) = f a
    bifoldMap1 _ g (Right b) = g b
    {-# INLINE bifoldMap1 #-}

instance Bifoldable1 (,) where
    bifoldMap1 f g (a, b) = f a <> g b
    {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,) x) where
    bifoldMap1 f g (_,a,b) = f a <> g b
    {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,,) x y) where
    bifoldMap1 f g (_,_,a,b) = f a <> g b
    {-# INLINE bifoldMap1 #-}

instance Bifoldable1 ((,,,,) x y z) where
    bifoldMap1 f g (_,_,_,a,b) = f a <> g b
    {-# INLINE bifoldMap1 #-}

instance Bifoldable1 Const where
    bifoldMap1 f _ (Const a) = f a
    {-# INLINE bifoldMap1 #-}

#ifdef MIN_VERSION_tagged
instance Bifoldable1 Tagged where
    bifoldMap1 _ g (Tagged b) = g b
    {-# INLINE bifoldMap1 #-}
#endif
