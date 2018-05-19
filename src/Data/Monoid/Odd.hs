{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Data.Monoid.Odd
Description : The monoid of 'Bool' under xor.
Copyright   : (c) Donnacha Oisín Kidney, 2018
License     : MIT
Maintainer  : mail@doisinkidney.com
Stability   : experimental
Portability : GHC
-}

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

-- | A monoid over XOR.
newtype Odd = Odd
    { getOdd :: Bool
    } deriving (Eq,Ord,Show,Bounded,Enum,Data,Read,Ix,Generic
               ,FiniteBits,Bits,Storable,NFData,Typeable)

instance Semigroup Odd where
    (<>) = (coerce :: (Bool -> Bool -> Bool) -> (Odd -> Odd -> Odd)) (/=)
    {-# INLINE (<>) #-}
    stimes n (Odd x) = Odd (x && n `mod` 2 == 1)
    {-# INLINE stimes #-}

instance Monoid Odd where
    mappend = (<>)
    {-# INLINE mappend #-}
    mempty = Odd False
    {-# INLINE mempty #-}

instance Group Odd where
    invert = id
    {-# INLINE invert #-}
