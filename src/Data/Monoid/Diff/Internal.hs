{-# options_ghc -fno-warn-noncanonical-monoid-instances #-}

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Monoid.Diff.Internal where

import           Data.Group                 (Group (..))
import           Data.Monoid.Odd
import           Data.Semigroup             (Semigroup (..))

import           Control.DeepSeq            (NFData (rnf))
import           Data.Data                  (Data, Typeable)
import           Data.Functor.Classes       (Read1 (liftReadPrec),
                                             Show1 (liftShowsPrec))
import           Data.Ix                    (Ix)
import           GHC.Generics               (Generic, Generic1)
import           Text.Read                  (Lexeme (Symbol), lift, parens,
                                             prec, step)
import           Text.Read.Lex              (expect)

import           Control.Applicative        (liftA2)
import           Control.Monad.Fix          (MonadFix (..))
import           Control.Monad.Zip          (MonadZip (..))

import           Data.Distributive          (Distributive (..))
import           Data.Functor.Adjunction
import           Data.Functor.Rep           (Representable (..))

import           Data.Functor.Apply         (Apply (..))
import           Data.Functor.Bind          (Bind (..))
import           Data.Functor.Extend        (Extend (..))
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Semigroup.Foldable    (Foldable1 (..))
import           Data.Semigroup.Traversable (Traversable1 (..))

import           Control.Comonad            (Comonad (..), ComonadApply (..))
import           Control.Comonad.Env.Class  (ComonadEnv)

import           Data.Bool                  (bool)
import           Data.Foldable              (Foldable (..))

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
    _ *> ys = ys
    {-# INLINE (*>) #-}
    xs <* _ = xs
    {-# INLINE (<*) #-}

instance Monad Diff where
    return = pure
    {-# INLINE return #-}
    (xx :-: xy) >>= f = x :-: y
      where
        x :-: _ = f xx
        _ :-: y = f xy
    {-# INLINE (>>=) #-}

instance Bind Diff where
    (xx :-: xy) >>- f = x :-: y
      where
        x :-: _ = f xx
        _ :-: y = f xy
    {-# INLINE (>>-) #-}

instance Semigroup a =>
         Semigroup (Diff a) where
    (xp :-: xn) <> (yp :-: yn) = (xp <> yp) :-: (xn <> yn)
    {-# INLINE (<>) #-}
    stimes n (x :-: y) = stimes n x :-: stimes n y
    {-# INLINE stimes #-}

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
    munzip ((xx,xy) :-: (yx,yy)) = (xx :-: yx, xy :-: yy)
    {-# INLINE munzip #-}

instance NFData a => NFData (Diff a) where
    rnf (x :-: y) = rnf x `seq` rnf y

instance Distributive Diff where
    distribute f =
        fmap (\(x :-: _) -> x) f :-: fmap (\(_ :-: y) -> y) f
    {-# INLINE distribute #-}
    collect g f =
        fmap (\xs -> case g xs of (x :-: _) -> x) f :-:
        fmap (\ys -> case g ys of (_ :-: y) -> y) f
    {-# INLINE collect #-}

instance Representable Diff where
    type Rep Diff = Bool
    tabulate f = f False :-: f True
    {-# INLINE tabulate #-}
    index (x :-: y) = bool x y
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

instance Apply Diff where
    (fx :-: fy) <.> (xx :-: xy) = fx xx :-: fy xy
    {-# INLINE (<.>) #-}
    liftF2 f (xx :-: xy) (yx :-: yy) = f xx yx :-: f xy yy
    {-# INLINE liftF2 #-}
    _ .> ys = ys
    {-# INLINE (.>) #-}
    xs <. _ = xs
    {-# INLINE (<.) #-}

instance Comonad Diff where
    extract (x :-: _) = x
    {-# INLINE extract #-}
    duplicate (x :-: y) = (x :-: y) :-: (y :-: x)
    {-# INLINE duplicate #-}
    extend f xy@(x :-: y) = f xy :-: f (y :-: x)
    {-# INLINE extend #-}

instance ComonadApply Diff where
    (fx :-: fy) <@> (xx :-: xy) = fx xx :-: fy xy
    {-# INLINE (<@>) #-}
    _ @> ys = ys
    {-# INLINE (@>) #-}
    xs <@ _ = xs
    {-# INLINE (<@) #-}

instance Bounded a => Bounded (Diff a) where
    minBound = minBound :-: maxBound
    {-# INLINE minBound #-}
    maxBound = maxBound :-: minBound
    {-# INLINE maxBound #-}

instance Show1 Diff where
    liftShowsPrec s _ d (xs :-: ys) =
        showParen (d > 6) $ s 7 xs . showString " :-: " . s 7 ys

instance Read1 Diff where
    liftReadPrec rp _ =
        parens $
        prec
            6
            (liftA2 (:-:) (step rp) (lift (expect (Symbol ":-:")) *> step rp))

instance Extend Diff where
    duplicated (x :-: y) = (x :-: y) :-: (y :-: x)
    {-# INLINE duplicated #-}
    extended f xy@(x :-: y) = f xy :-: f (y :-: x)
    {-# INLINE extended #-}

newtype Parity a = Parity
    { runParity :: (Odd, a)
    } deriving (Functor,Foldable,Traversable,Foldable1,Applicative
               ,Monad,Apply,Bind,Extend,Comonad,ComonadApply,Eq,Ord
               ,Show,Read,Bounded,Ix,Semigroup,Monoid,NFData
               ,ComonadEnv Odd)

instance Traversable1 Parity where
    traverse1 f (Parity (x, y)) = fmap (\y' -> Parity (x, y')) (f y)
    {-# INLINE traverse1 #-}
    sequence1 (Parity (x, y)) = fmap (\y' -> Parity (x, y')) y
    {-# INLINE sequence1 #-}

instance MonadFix Parity where
    mfix f = let (p,x) = runParity (f x) in Parity (p,x)

instance Adjunction Parity Diff where
    leftAdjunct f a = f (Parity (Odd False, a)) :-: f (Parity (Odd True, a))
    {-# INLINE leftAdjunct #-}
    unit a = Parity (Odd False, a) :-: Parity (Odd True, a)
    {-# INLINE unit #-}
    rightAdjunct f (Parity (Odd False,a)) =
        case f a of
            x :-: _ -> x
    rightAdjunct f (Parity (Odd True,a)) =
        case f a of
            _ :-: x -> x
    {-# INLINE rightAdjunct #-}
    counit (Parity (Odd False,x :-: _)) = x
    counit (Parity (Odd True,_ :-: x)) = x
    {-# INLINE counit #-}
