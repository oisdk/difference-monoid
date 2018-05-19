{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Monoid.Odd where

import           Data.Coerce      (coerce)

import           Data.Group
import           Data.Semigroup   (Semigroup (stimes, (<>)))

import           Control.DeepSeq  (NFData)
import           Data.Bits        (Bits, FiniteBits)
import           Data.Data        (Data, Typeable)
import           Data.Ix          (Ix)
import           Foreign.Storable (Storable)
import           GHC.Generics     (Generic)

newtype Odd = Odd
    { getOdd :: Bool
    } deriving (Eq,Ord,Show,Bounded,Enum,Data,Read,Ix,Generic
               ,FiniteBits,Bits,Storable,NFData,Typeable)

-- |
-- prop> (x <> y) <> z === x <> (y <> z :: Odd)
-- prop> \(NonNegative n) -> stimesMonoid n (x :: Odd) === stimes n x
-- prop> (x <> y) === (y <> x :: Odd)
instance Semigroup Odd where
    (<>) = (coerce :: (Bool -> Bool -> Bool) -> (Odd -> Odd -> Odd)) (/=)
    {-# INLINE (<>) #-}
    stimes n (Odd x) = Odd (x && n `mod` 2 == 1)
    {-# INLINE stimes #-}

-- |
-- prop> (x <> mempty) === (x :: Odd)
instance Monoid Odd where
    mappend = (<>)
    {-# INLINE mappend #-}
    mempty = Odd False
    {-# INLINE mempty #-}

-- |
-- prop> (x <> invert x) === (mempty :: Odd)
instance Group Odd where
    invert = id
    {-# INLINE invert #-}


-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Semigroup (stimesMonoid)
-- >>> :{
-- instance Arbitrary Odd where arbitrary = fmap Odd arbitrary
-- :}
