{-# options_ghc -fno-warn-noncanonical-monoid-instances #-}

module Data.Monoid.Diff where

import Data.Semigroup
import Data.Group

infixl 6 :-:
data Diff a = !a :-: !a deriving Show

instance Functor Diff where
    fmap f (x :-: y) = f x :-: f y

instance Applicative Diff where
    pure x = x :-: x
    (fx :-: fy) <*> (xx :-: xy) = fx xx :-: fy xy

instance Monad Diff where
    (xx :-: xy) >>= f = x :-: y
      where
        x :-: _ = f xx
        _ :-: y = f xy

instance Semigroup a =>
         Semigroup (Diff a) where
    (xp :-: xn) <> (yp :-: yn) = (xp <> yp) :-: (xn <> yn)

instance (Monoid a) =>
         Monoid (Diff a) where
    mappend (xp :-: xn) (yp :-: yn) = (xp `mappend` yp) :-: (xn `mappend` yn)
    mempty = mempty :-: mempty

instance Monoid a => Group (Diff a) where
    invert (x :-: y) = y :-: x

instance (Eq a, Monoid a) =>
         Eq (Diff a) where
    (xp :-: xn) == (yp :-: yn) = mappend xn yp == mappend xp yn

instance (Ord a, Monoid a) =>
         Ord (Diff a) where
    compare (xp :-: xn) (yp :-: yn) = compare (mappend yn xp) (mappend xn yp)

diff :: Monoid a => a -> Diff a
diff x = x :-: mempty

retract :: Group a => Diff a -> a
retract (x :-: y) = x `mappend` invert y

normalize :: (a -> a -> (a, a)) -> Diff a -> Diff a
normalize f (x :-: y) = uncurry (:-:) (f x y)
