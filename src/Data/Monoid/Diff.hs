{-|
Module      : Data.Monoid.Diff
Description : The Difference Monoid, to add subtraction to arbitrary monoids.
Copyright   : (c) Donnacha Oisín Kidney, 2018
License     : MIT
Maintainer  : mail@doisinkidney.com
Stability   : experimental
Portability : GHC

This module provides the Difference Monoid, which adds subtraction to
arbitrary monoids.

This has a number of uses:

* @'Diff' ('Data.Monoid.Product' a)@ will give you a type similar to
'Data.Ratio'. Here, the "subtraction" operation is division. For
example:

    >>> (1 :-: 2) <> (3 :-: 4) :: Diff (Product Int)
    Product {getProduct = 3} :-: Product {getProduct = 8}

* In a similar vein, @'Diff' ('Data.Monoid.Sum' a)@ will add subtraction
to a numeric type:

    >>> runDiff (-) (diff 2 <> diff 3 <> invert (diff 4)) :: Sum Natural
    Sum {getSum = 1}

    This will let you work with nonnegative types, where you need some
    form of subtraction (for, e.g., differences, hence the name), and
    you only want to check for underflow once.

* Using the above example, in particular, we get a monoid for averages:

    >>> import Data.Function (on)
    >>> let avg = runDiff ((%) `on` getProduct.getSum) . foldMap (fmap Sum . diff . Product)
    >>> avg [1,4,3,2,5]
    3 % 1

The 'Monoid' and 'Data.Semigroup.Semigroup' laws hold in a pretty
straightforward way, provided the underlying type also follows those
laws.

For the 'Data.Group.Group' laws, the underlying type must be a
cancellative semigroup.

A cancellative semigroup is one where

* @a 'Data.Semigroup.<>' b = a 'Data.Semigroup.<>' c@ implies @b = c@
* @b 'Data.Semigroup.<>' a = c 'Data.Semigroup.<>' a@ implies @b = c@

If this does not hold, than the equivalence only holds modulo the
the addition of some constant

Most common semigroups are cancellative, however notable
exceptions include the cross product of vectors, matrix
multiplication, and sets:

@'Data.Set.fromList' [1] 'Data.Semigroup.<>' 'Data.Set.fromList' [1,2] = 'Data.Set.fromList' [1] 'Data.Semigroup.<>' 'Data.Set.fromList' [2]@

This type is known formally as the <https://en.wikipedia.org/wiki/Grothendieck_group Grothendieck group>.
-}
module Data.Monoid.Diff
  (
   -- * The Diff Type
   Diff(..)
  ,
   -- * Functions for working with 'Diff'
   diff
  ,retract
  ,foldDiff
  ,runDiff
  ,normalize
  -- * Re-Exports from Group
  ,Group(..)
  )
  where

import           Data.Monoid.Diff.Internal
import           Data.Group (Group(..))

-- $setup
-- >>> import Data.Monoid
-- >>> import Numeric.Natural
-- >>> import Data.Group
-- >>> import Data.Ratio
