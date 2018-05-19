{-# options_ghc -fno-warn-noncanonical-monoid-instances #-}

{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Monoid.Diff where

import           Data.Group
import           Data.Semigroup

import           GHC.Generics        (Generic, Generic1)
import           Control.DeepSeq     (NFData (rnf))
import           Data.Data

import           Control.Applicative
import           Control.Monad.Fix
import           Control.Monad.Zip

import           Data.Functor.Rep
import           Data.Distributive

import           Data.Semigroup.Foldable
import           Data.Semigroup.Traversable
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Functor.Apply

import           Data.Foldable

infixl 6 :-:
data Diff a =
    !a :-: !a
    deriving (Show,Read,Data,Typeable,Generic,Generic1)

instance Functor Diff where
    fmap f (x :-: y) = f x :-: f y
    {-# INLINE fmap #-}
    x <$ _ = x :-: x
    {-# INLINE (<$) #-}

instance Foldable Diff where
    fold (x :-: y) = x `mappend` y
    {-# INLINE fold #-}
    foldMap f (x :-: y) = f x `mappend` f y
    {-# INLINE foldMap #-}
    foldr f b (x :-: y) = f x (f y b)
    {-# INLINE foldr #-}
    foldr' f !b (x :-: y) = case f y b of
      !b' -> f x b'
    {-# INLINE foldr' #-}
    foldl f b (x :-: y) = f (f b x) y
    {-# INLINE foldl #-}
    foldl' f !b (x :-: y) = case f b x of
      !b' -> f b' y
    minimum (x :-: y) = min x y
    {-# INLINE minimum #-}
    maximum (x :-: y) = max x y
    {-# INLINE maximum #-}
    foldr1 f (x :-: y) = f x y
    {-# INLINE foldr1 #-}
    foldl1 f (x :-: y) = f x y
    {-# INLINE foldl1 #-}
    toList (x :-: y) = [x,y]
    {-# INLINE toList #-}
    null _ = False
    {-# INLINE null #-}
    length _ = 2
    {-# INLINE length #-}
    elem x (y :-: z) = x == y || x == z
    {-# INLINE elem #-}
    sum (x :-: y) = x + y
    {-# INLINE sum #-}
    product (x :-: y) = x * y
    {-# INLINE product #-}

instance Traversable Diff where
    traverse f (x :-: y) = liftA2 (:-:) (f x) (f y)
    {-# INLINE traverse #-}
    sequenceA (x :-: y) = liftA2 (:-:) x y
    {-# INLINE sequenceA #-}

instance Applicative Diff where
    pure x = x :-: x
    {-# INLINE pure #-}
    (fx :-: fy) <*> (xx :-: xy) = fx xx :-: fy xy
    {-# INLINE (<*>) #-}
    liftA2 f (xx :-: xy) (yx :-: yy) = f xx yx :-: f xy yy
    {-# INLINE liftA2 #-}

instance Monad Diff where
    return = pure
    {-# INLINE return #-}
    (xx :-: xy) >>= f = x :-: y
      where
        x :-: _ = f xx
        _ :-: y = f xy
    {-# INLINE (>>=) #-}

instance Semigroup a =>
         Semigroup (Diff a) where
    (xp :-: xn) <> (yp :-: yn) = (xp <> yp) :-: (xn <> yn)
    {-# INLINE (<>) #-}

instance (Monoid a) =>
         Monoid (Diff a) where
    mappend (xp :-: xn) (yp :-: yn) = (xp `mappend` yp) :-: (xn `mappend` yn)
    {-# INLINE mappend #-}
    mempty = mempty :-: mempty
    {-# INLINE mempty #-}

instance Monoid a => Group (Diff a) where
    invert (x :-: y) = y :-: x
    {-# INLINE invert #-}

instance (Eq a, Semigroup a) =>
         Eq (Diff a) where
    (xp :-: xn) == (yp :-: yn) = xn <> yp == xp <> yn
    {-# INLINE (==) #-}

instance (Ord a, Semigroup a) =>
         Ord (Diff a) where
    compare (xp :-: xn) (yp :-: yn) = compare (yn <> xp) (xn <> yp)
    {-# INLINE compare #-}

diff :: Monoid a => a -> Diff a
diff x = x :-: mempty
{-# INLINE diff #-}

retract :: Group a => Diff a -> a
retract (x :-: y) = x `mappend` invert y
{-# INLINE retract #-}

foldDiff :: Group b => (a -> b) -> Diff a -> b
foldDiff f (x :-: y) = f x `mappend` invert (f y)
{-# INLINE foldDiff #-}

normalize :: (a -> a -> (a, a)) -> Diff a -> Diff a
normalize f (x :-: y) = uncurry (:-:) (f x y)
{-# INLINE normalize #-}

instance MonadFix Diff where
    mfix f  = (let n :-: _ = f n in n) :-: (let _ :-: d = f d in d)

instance MonadZip Diff where
    mzipWith = liftA2
    {-# INLINE mzipWith #-}

instance NFData a => NFData (Diff a) where
    rnf (x :-: y) = rnf x `seq` rnf y

instance Distributive Diff where
  distribute f = fmap (\(x :-: _) -> x) f :-: fmap (\(_ :-: y) -> y) f
  {-# INLINE distribute #-}

instance Representable Diff where
    type Rep Diff = Bool
    tabulate f = f False :-: f True
    {-# INLINE tabulate #-}
    index (x :-: _) False = x
    index (_ :-: y) True  = y
    {-# INLINE index #-}

instance Foldable1 Diff where
    foldMap1 f (x :-: y) = f x <> f y
    {-# INLINE foldMap1 #-}
    fold1 (x :-: y) = x <> y
    {-# INLINE fold1 #-}
    toNonEmpty (x :-: y) = x :| [y]
    {-# INLINE toNonEmpty #-}

instance Traversable1  Diff where
    traverse1 f (x :-: y) = liftF2 (:-:) (f x) (f y)
    {-# INLINE traverse1 #-}
    sequence1 (x :-: y) = liftF2 (:-:) x y
    {-# INLINE sequence1 #-}
