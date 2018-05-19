# difference-monoid

This package provides the Difference Monoid, which adds subtraction to
arbitrary monoids.

This has a number of uses:

* `Diff (Product a)` will give you a type similar to
Data.Ratio. Here, the "subtraction" operation is division. For
example:

    ```haskell
    >>> (1 :-: 2) <> (3 :-: 4) :: Diff (Product Int)
    Product {getProduct = 3} :-: Product {getProduct = 8}
    ```

* In a similar vein, `Diff (Sum a)` will add subtraction
to a numeric type:

    ```haskell
    >>> runDiff (-) (diff 2 <> diff 3 <> invert (diff 4)) :: Sum Natural
    Sum {getSum = 1}
    ```

    This will let you work with nonnegative types, where you need some
    form of subtraction (for, e.g., differences, hence the name), and
    you only want to check for underflow once.

* Using the above example, in particular, we get a monoid for averages:

    ```haskell
    >>> import Data.Function (on)
    >>> let avg = runDiff ((%) `on` getProduct.getSum) . foldMap (fmap Sum . diff . Product)
    >>> avg [1,4,3,2,5]
    3 % 1
    ```

The Monoid and Semigroup laws hold in a pretty
straightforward way, provided the underlying type also follows those
laws.

For the Group laws, the underlying type must be a
cancellative semigroup.

A cancellative semigroup is one where

* `a <> b = a <> c` implies `b = c`
* `b <> a = c <> a` implies `b = c`

If this does not hold, than the equivalence only holds modulo the
the addition of some constant

Most common semigroups are cancellative, however notable
exceptions include the cross product of vectors, matrix
multiplication, and sets:

```haskell
fromList [1] <> fromList [1,2] = fromList [1] <> fromList [2]`
```

This type is known formally as the [Grothendieck group](https://en.wikipedia.org/wiki/Grothendieck_group).

The package also provides the `Parity` monad and comonad, which is left-adjunct to the difference monoid.
