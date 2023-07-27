{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeOperators              #-}

#if __GLASGOW_HASKELL__ >=706
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >=702
{-# LANGUAGE Trustworthy                #-}
#endif

-- | A class of non-empty data structures that can be folded to a summary value.
module Data.Foldable1 (
    Foldable1(..),
    foldr1, foldr1',
    foldl1, foldl1',
    intercalate1,
    foldrM1,
    foldlM1,
    foldrMapM1,
    foldlMapM1,
    maximumBy,
    minimumBy,
    ) where

import Data.Foldable      (Foldable, foldlM, foldr)
import Data.List          (foldl, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
       (Dual (..), First (..), Last (..), Max (..), Min (..), Product (..),
       Semigroup (..), Sum (..))
import Prelude
       (Maybe (..), Monad (..), Ord, Ordering (..), id, seq, ($!), ($), (.),
       (=<<), flip, const, error)

import qualified Data.List.NonEmpty as NE

#if MIN_VERSION_base(4,4,0)
import Data.Complex (Complex (..))
import GHC.Generics
       (M1 (..), Par1 (..), Rec1 (..), V1, (:*:) (..), (:+:) (..), (:.:) (..))
#else
import Generics.Deriving
       (M1 (..), Par1 (..), Rec1 (..), V1, (:*:) (..), (:+:) (..), (:.:) (..))
#endif

#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down (..))
#endif

#if MIN_VERSION_base(4,8,0)
import qualified Data.Monoid as Mon
#endif

#if !MIN_VERSION_base(4,12,0)
import Data.Orphans ()
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged (Tagged (..))
#endif

#ifdef MIN_VERSION_ghc_prim
#if MIN_VERSION_ghc_prim(0,7,0)
import GHC.Tuple (Solo (..))
#endif
#endif

-- Instances
import Control.Applicative.Backwards (Backwards (..))
import Control.Applicative.Lift      (Lift (..))
import Control.Monad.Trans.Identity  (IdentityT (..))
import Data.Functor.Compose          (Compose (..))
import Data.Functor.Identity         (Identity (..))
import Data.Functor.Reverse          (Reverse (..))
import Data.Tree                     (Tree (..))

import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum     as Functor

-- coerce
#if __GLASGOW_HASKELL__ <708
import Unsafe.Coerce (unsafeCoerce)
#else
import Data.Coerce (Coercible, coerce)
#endif

-- $setup
-- >>> import Prelude hiding (foldr1, foldl1, head, last, minimum, maximum)

-------------------------------------------------------------------------------
-- Foldable1 type class
-------------------------------------------------------------------------------

-- | Non-empty data structures that can be folded.
class Foldable t => Foldable1 t where
#if __GLASGOW_HASKELL__ >= 708
    {-# MINIMAL foldMap1 | foldrMap1 #-}
#endif

    -- At some point during design it was possible to define this class using
    -- only 'toNonEmpty'. But it seems a bad idea in general.
    --
    -- So currently we require either foldMap1 or foldrMap1
    --
    -- * foldMap1 defined using foldrMap1
    -- * foldrMap1 defined using foldMap1
    --
    -- One can alsays define instance using following pattern:
    --
    --     toNonEmpty = ...
    --     foldMap f     = foldMap f     . toNonEmpty
    --     foldrMap1 f g = foldrMap1 f g . toNonEmpty

    -- | Combine the elements of a structure using a semigroup.
    fold1 :: Semigroup m => t m -> m
    fold1 = foldMap1 id

    -- | Map each element of the structure to a semigroup,
    -- and combine the results.
    --
    -- >>> foldMap1 Sum (1 :| [2, 3, 4])
    -- Sum {getSum = 10}
    --
    foldMap1 :: Semigroup m => (a -> m) -> t a -> m
    foldMap1 f = foldrMap1 f (\a m -> f a <> m)

    -- | A variant of 'foldMap1' that is strict in the accumulator.
    --
    -- >>> foldMap1' Sum (1 :| [2, 3, 4])
    -- Sum {getSum = 10}
    --
    foldMap1' :: Semigroup m => (a -> m) -> t a -> m
    foldMap1' f = foldlMap1' f (\m a -> m <> f a)

    -- | List of elements of a structure, from left to right.
    --
    -- >>> toNonEmpty (Identity 2)
    -- 2 :| []
    --
    toNonEmpty :: t a -> NonEmpty a
    toNonEmpty = runNonEmptyDList . foldMap1 singleton

    -- | The largest element of a non-empty structure.
    --
    -- >>> maximum (32 :| [64, 8, 128, 16])
    -- 128
    --
    maximum :: Ord a => t a -> a
    maximum = getMax #. foldMap1' Max

    -- | The least element of a non-empty structure.
    --
    -- >>> minimum (32 :| [64, 8, 128, 16])
    -- 8
    --
    minimum :: Ord a => t a -> a
    minimum = getMin #. foldMap1' Min

    -- | The first element of a non-empty structure.
    --
    -- >>> head (1 :| [2, 3, 4])
    -- 1
    --
    head :: t a -> a
    head = getFirst #. foldMap1 First

    -- | The last element of a non-empty structure.
    --
    -- >>> last (1 :| [2, 3, 4])
    -- 4
    --
    last :: t a -> a
    last = getLast #. foldMap1 Last

    -- | Generalized 'foldr1'.
    foldrMap1 :: (a -> b) -> (a -> b -> b) -> t a -> b
    foldrMap1 f g xs =
        appFromMaybe (foldMap1 (FromMaybe #. h) xs) Nothing
      where
        h a Nothing  = f a
        h a (Just b) = g a b

    -- | Generalized 'foldl1''.
    foldlMap1' :: (a -> b) -> (b -> a -> b) -> t a -> b
    foldlMap1' f g xs =
        foldrMap1 f' g' xs SNothing
      where
        -- f' :: a -> SMaybe b -> b
        f' a SNothing  = f a
        f' a (SJust b) = g b a

        -- g' :: a -> (SMaybe b -> b) -> SMaybe b -> b
        g' a x SNothing  = x $! SJust (f a)
        g' a x (SJust b) = x $! SJust (g b a)

    -- | Generalized 'foldl1'.
    foldlMap1 :: (a -> b) -> (b -> a -> b) -> t a -> b
    foldlMap1 f g xs =
        appFromMaybe (getDual (foldMap1 ((Dual . FromMaybe) #. h) xs)) Nothing
      where
        h a Nothing  = f a
        h a (Just b) = g b a

    -- | Generalized 'foldr1''.
    foldrMap1' :: (a -> b) -> (a -> b -> b) -> t a -> b
    foldrMap1' f g xs =
        foldlMap1 f' g' xs SNothing
      where
        f' a SNothing  = f a
        f' a (SJust b) = g a b

        g' bb a SNothing  = bb $! SJust (f a)
        g' bb a (SJust b) = bb $! SJust (g a b)

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- | Right-associative fold of a structure.
--
-- In the case of lists, 'foldr1', when applied to a binary operator,
-- and a list, reduces the list using the binary operator,
-- from right to left:
--
-- > foldr1 f [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn1 `f` xn )...)
--
-- Note that, since the head of the resulting expression is produced by
-- an application of the operator to the first element of the list,
-- 'foldr1' can produce a terminating expression from an infinite list.
--
-- For a general 'Foldable1' structure this should be semantically identical
-- to,
--
-- @foldr1 f = foldr1 f . 'toNonEmpty'@
--
foldr1 :: Foldable1 t => (a -> a -> a) -> t a -> a
foldr1 = foldrMap1 id
{-# INLINE foldr1 #-}

-- | Right-associative fold of a structure, but with strict application of
-- the operator.
--
foldr1' :: Foldable1 t => (a -> a -> a) -> t a -> a
foldr1' = foldrMap1' id
{-# INLINE foldr1' #-}

-- | Left-associative fold of a structure.
--
-- In the case of lists, 'foldl1', when applied to a binary
-- operator, and a list, reduces the list using the binary operator,
-- from left to right:
--
-- > foldl1 f [x1, x2, ..., xn] == (...((x1 `f` x2) `f`...) `f` xn
--
-- Note that to produce the outermost application of the operator the
-- entire input list must be traversed. This means that 'foldl1' will
-- diverge if given an infinite list.
--
-- Also note that if you want an efficient left-fold, you probably want to
-- use 'foldl1'' instead of 'foldl1'. The reason for this is that latter does
-- not force the "inner" results (e.g. @x1 \`f\` x2@ in the above example)
-- before applying them to the operator (e.g. to @(\`f\` x3)@). This results
-- in a thunk chain \(\mathcal{O}(n)\) elements long, which then must be
-- evaluated from the outside-in.
--
-- For a general 'Foldable1' structure this should be semantically identical
-- to,
--
-- @foldl1 f z = foldl1 f . 'toNonEmpty'@
--
foldl1 :: Foldable1 t => (a -> a -> a) -> t a -> a
foldl1 = foldlMap1 id
{-# INLINE foldl1 #-}

-- | Left-associative fold of a structure but with strict application of
-- the operator.
--
-- This ensures that each step of the fold is forced to weak head normal
-- form before being applied, avoiding the collection of thunks that would
-- otherwise occur. This is often what you want to strictly reduce a finite
-- list to a single, monolithic result (e.g. 'length').
--
-- For a general 'Foldable1' structure this should be semantically identical
-- to,
--
-- @foldl1' f z = foldl1 f . 'toNonEmpty'@
--
foldl1' :: Foldable1 t => (a -> a -> a) -> t a -> a
foldl1' = foldlMap1' id
{-# INLINE foldl1' #-}

-- | Insert an @m@ between each pair of @t m@.
--
-- >>> intercalate1 ", " $ "hello" :| ["how", "are", "you"]
-- "hello, how, are, you"
--
-- >>> intercalate1 ", " $ "hello" :| []
-- "hello"
--
-- >>> intercalate1 mempty $ "I" :| ["Am", "Fine", "You?"]
-- "IAmFineYou?"
--
intercalate1 :: (Foldable1 t, Semigroup m) => m -> t m -> m
intercalate1 = flip intercalateMap1 id

intercalateMap1 :: (Foldable1 t, Semigroup m) => m -> (a -> m) -> t a -> m
intercalateMap1 j f = flip joinee j . foldMap1 (JoinWith . const . f)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the right, i.e. from right to left.
foldrM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldrM1 = foldrMapM1 return

-- | Map variant of 'foldrM1'.
foldrMapM1 :: (Foldable1 t, Monad m) => (a -> m b) -> (a -> b -> m b) -> t a -> m b
foldrMapM1 g f = go . toNonEmpty
  where
    go (e:|es) =
      case es of
        []   -> g e
        x:xs -> f e =<< go (x:|xs)

-- | Monadic fold over the elements of a non-empty structure,
-- associating to the left, i.e. from left to right.
foldlM1 :: (Foldable1 t, Monad m) => (a -> a -> m a) -> t a -> m a
foldlM1 = foldlMapM1 return

-- | Map variant of 'foldlM1'.
foldlMapM1 :: (Foldable1 t, Monad m) => (a -> m b) -> (b -> a -> m b) -> t a -> m b
foldlMapM1 g f t = g x >>= \y -> foldlM f y xs
  where x:|xs = toNonEmpty t

-- | The largest element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
maximumBy :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldl1' max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y

-- | The least element of a non-empty structure with respect to the
-- given comparison function.

-- See Note [maximumBy/minimumBy space usage]
minimumBy :: Foldable1 t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldl1' min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x

-------------------------------------------------------------------------------
-- Auxiliary types
-------------------------------------------------------------------------------

-- | Used for default toNonEmpty implementation.
newtype NonEmptyDList a = NEDL { unNEDL :: [a] -> NonEmpty a }

instance Semigroup (NonEmptyDList a) where
  xs <> ys = NEDL (unNEDL xs . NE.toList . unNEDL ys)
  {-# INLINE (<>) #-}

-- | Create dlist with a single element
singleton :: a -> NonEmptyDList a
singleton = NEDL #. (:|)

-- | Convert a dlist to a non-empty list
runNonEmptyDList :: NonEmptyDList a -> NonEmpty a
runNonEmptyDList = ($ []) . unNEDL
{-# INLINE runNonEmptyDList #-}

-- | Used for foldrMap1 and foldlMap1 definitions
newtype FromMaybe b = FromMaybe { appFromMaybe :: Maybe b -> b }

instance Semigroup (FromMaybe b) where
    FromMaybe f <> FromMaybe g = FromMaybe (f . Just . g)

-- | Strict maybe, used to implement default foldlMap1' etc.
data SMaybe a = SNothing | SJust !a

-- | Used to implement intercalate1/Map
newtype JoinWith a = JoinWith {joinee :: (a -> a)}

instance Semigroup a => Semigroup (JoinWith a) where
  JoinWith a <> JoinWith b = JoinWith $ \j -> a j <> j <> b j

-------------------------------------------------------------------------------
-- Instances for misc base types
-------------------------------------------------------------------------------

instance Foldable1 NonEmpty where
    foldMap1 f (x :| xs) = go (f x) xs where
        go y [] = y
        go y (z : zs) = y <> go (f z) zs

    foldMap1' f (x :| xs) = foldl' (\m y -> m <> f y) (f x) xs

    toNonEmpty = id

    foldrMap1 g f (x :| xs) = go x xs where
        go y [] = g y
        go y (z : zs) = f y (go z zs)

    foldlMap1  g f (x :| xs) = foldl f (g x) xs
    foldlMap1' g f (x :| xs) = let gx = g x in gx `seq` foldl' f gx xs

    head = NE.head
    last = NE.last

#if MIN_VERSION_base(4,6,0)
instance Foldable1 Down where
    foldMap1 = coerce
#endif

#if MIN_VERSION_base(4,4,0)
instance Foldable1 Complex where
    foldMap1 f (x :+ y) = f x <> f y

    toNonEmpty (x :+ y) = x :| y : []
#endif

-------------------------------------------------------------------------------
-- Instances for tuples
-------------------------------------------------------------------------------

-- 3+ tuples are not Foldable/Traversable

instance Foldable1 ((,) a) where
    foldMap1 f (_, y) = f y
    toNonEmpty (_, x) = x :| []
    minimum (_, x) = x
    maximum (_, x) = x
    head (_, x) = x
    last (_, x) = x

-------------------------------------------------------------------------------
-- Monoid / Semigroup instances
-------------------------------------------------------------------------------

instance Foldable1 Dual where
    foldMap1 = coerce

instance Foldable1 Sum where
    foldMap1 = coerce

instance Foldable1 Product where
    foldMap1 = coerce

instance Foldable1 Min where
    foldMap1 = coerce

instance Foldable1 Max where
    foldMap1 = coerce

instance Foldable1 First where
    foldMap1 = coerce

instance Foldable1 Last where
    foldMap1 = coerce

#if MIN_VERSION_base(4,8,0)
deriving instance (Foldable1 f) => Foldable1 (Mon.Alt f)
#endif

#if MIN_VERSION_base(4,12,0)
deriving instance (Foldable1 f) => Foldable1 (Mon.Ap f)
#endif

-------------------------------------------------------------------------------
-- GHC.Generics instances
-------------------------------------------------------------------------------

instance Foldable1 V1 where
    foldMap1 _ x = x `seq` error "foldMap1 @V1"

instance Foldable1 Par1 where
    foldMap1 = coerce

deriving instance Foldable1 f => Foldable1 (Rec1 f)

deriving instance Foldable1 f => Foldable1 (M1 i c f)

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :+: g) where
    foldMap1 f (L1 x) = foldMap1 f x
    foldMap1 f (R1 y) = foldMap1 f y

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :*: g) where
    foldMap1 f (x :*: y) = foldMap1 f x <> foldMap1 f y

instance (Foldable1 f, Foldable1 g) => Foldable1 (f :.: g) where
    foldMap1 f = foldMap1 (foldMap1 f) . unComp1

-------------------------------------------------------------------------------
-- Extra instances
-------------------------------------------------------------------------------

instance Foldable1 Identity where
    foldMap1      = coerce

    foldrMap1  g _ = coerce g
    foldrMap1' g _ = coerce g
    foldlMap1  g _ = coerce g
    foldlMap1' g _ = coerce g

    toNonEmpty (Identity x) = x :| []

    last    = coerce
    head    = coerce
    minimum = coerce
    maximum = coerce

-- | It would be enough for either half of a product to be 'Foldable1'.
-- Other could be 'Foldable'.
instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Product f g) where
    foldMap1 f (Functor.Pair x y)    = foldMap1 f x <> foldMap1 f y
    foldrMap1 g f (Functor.Pair x y) = foldr f (foldrMap1 g f y) x

    head (Functor.Pair x _) = head x
    last (Functor.Pair _ y) = last y

instance (Foldable1 f, Foldable1 g) => Foldable1 (Functor.Sum f g) where
    foldMap1 f (Functor.InL x) = foldMap1 f x
    foldMap1 f (Functor.InR y) = foldMap1 f y

    foldrMap1 g f (Functor.InL x) = foldrMap1 g f x
    foldrMap1 g f (Functor.InR y) = foldrMap1 g f y

    toNonEmpty (Functor.InL x) = toNonEmpty x
    toNonEmpty (Functor.InR y) = toNonEmpty y

    head (Functor.InL x) = head x
    head (Functor.InR y) = head y
    last (Functor.InL x) = last x
    last (Functor.InR y) = last y

    minimum (Functor.InL x) = minimum x
    minimum (Functor.InR y) = minimum y
    maximum (Functor.InL x) = maximum x
    maximum (Functor.InR y) = maximum y

instance (Foldable1 f, Foldable1 g) => Foldable1 (Compose f g) where
    foldMap1 f = foldMap1 (foldMap1 f) . getCompose

    foldrMap1 f g = foldrMap1 (foldrMap1 f g) (\xs x -> foldr g x xs) . getCompose

    head = head . head . getCompose
    last = last . last . getCompose

-------------------------------------------------------------------------------
-- containers
-------------------------------------------------------------------------------

instance Foldable1 Tree where
    foldMap1 f (Node x [])       = f x
    foldMap1 f (Node x (y : ys)) = f x <> foldMap1 (foldMap1 f) (y :| ys)

    foldMap1' f = go where
        go (Node x ys) =
            foldl' (\m zs -> let gozs = go zs in gozs `seq` m <> gozs) (f x) ys

    foldlMap1 f g (Node x xs) = goForest (f x) xs where
        goForest = foldl' go
        go y (Node z zs) = goForest (g y z) zs

    foldlMap1' f g (Node x xs) = goForest (f x) xs where
        goForest !y = foldl' go y
        go !y (Node z zs) = goForest (g y z) zs

    head (Node x _) = x

-------------------------------------------------------------------------------
-- transformers
-------------------------------------------------------------------------------

instance Foldable1 f => Foldable1 (Reverse f) where
    foldMap1 f = getDual . foldMap1 (Dual . f) . getReverse

    foldrMap1  f g (Reverse xs) = foldlMap1  f (flip g) xs
    foldlMap1  f g (Reverse xs) = foldrMap1  f (flip g) xs
    foldrMap1' f g (Reverse xs) = foldlMap1' f (flip g) xs
    foldlMap1' f g (Reverse xs) = foldrMap1' f (flip g) xs

    head = last . getReverse
    last = head . getReverse

deriving instance Foldable1 f => Foldable1 (IdentityT f)

instance Foldable1 f => Foldable1 (Backwards f) where
    foldMap1 f = foldMap1 f . forwards

instance Foldable1 f => Foldable1 (Lift f) where
    foldMap1 f (Pure x)  = f x
    foldMap1 f (Other y) = foldMap1 f y

-------------------------------------------------------------------------------
-- tagged
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_tagged
instance Foldable1 (Tagged b) where
    foldMap1      = coerce

    foldrMap1  g _ = coerce g
    foldrMap1' g _ = coerce g
    foldlMap1  g _ = coerce g
    foldlMap1' g _ = coerce g

    toNonEmpty x = coerce x :| []

    last    = coerce
    head    = coerce
    minimum = coerce
    maximum = coerce
#endif

-------------------------------------------------------------------------------
-- ghc-prim
-------------------------------------------------------------------------------

#ifdef MIN_VERSION_ghc_prim
#if MIN_VERSION_ghc_prim(0,7,0)
instance Foldable1 Solo where
    foldMap1 f (Solo y) = f y
    toNonEmpty (Solo x) = x :| []
    minimum (Solo x) = x
    maximum (Solo x) = x
    head (Solo x) = x
    last (Solo x) = x
#endif
#endif

-------------------------------------------------------------------------------
-- coerce shim
-------------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <708
coerce :: a -> b
coerce = unsafeCoerce

(#.) :: (b -> c) -> (a -> b) -> a -> c
(#.) _f = coerce
#else
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _f = coerce
#endif
