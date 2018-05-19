{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import           Hedgehog
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range
import           Hedgehog.Checkers

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map

import           Data.Function

import           Data.Monoid.Diff
import           Data.Semigroup
import           Data.Group


newtype FreeAbelian a = FreeAbelian
    { getFreeAbelian :: Map a Int
    } deriving (Eq, Ord, Show)

instance Ord a => Semigroup (FreeAbelian a) where
    FreeAbelian xs <> FreeAbelian ys = FreeAbelian (Map.unionWith (+) xs ys)

instance Ord a => Monoid (FreeAbelian a) where
    mempty = FreeAbelian Map.empty
    mappend = (<>)

freeAbelian :: MonadGen m => m (FreeAbelian Int)
freeAbelian =
    FreeAbelian <$>
    Gen.map
        (Range.linear 0 10)
        ((,) <$> Gen.int (Range.linear 1 1000) <*> Gen.int (Range.linear 1 1000))

prop_AbelianMonoid :: Property
prop_AbelianMonoid = property $ monoid freeAbelian

intDiff :: MonadGen m => m (Diff (Sum Int))
intDiff = ((:-:) `on` Sum) <$> Gen.int (Range.linear 0 10) <*> Gen.int (Range.linear 0 10)

freeDiff :: MonadGen m => m (Diff (FreeAbelian Int))
freeDiff = (:-:) <$> freeAbelian <*> freeAbelian

prop_DiffMonoid :: Property
prop_DiffMonoid = property $ monoid intDiff

prop_DiffCommutative :: Property
prop_DiffCommutative = property $ commutativity (<>) intDiff

prop_DiffInversion :: Property
prop_DiffInversion = property $ inversion (<>) mempty invert intDiff

prop_FreeDiffMonoid :: Property
prop_FreeDiffMonoid = property $ monoid freeDiff

prop_FreeDiffCommutative :: Property
prop_FreeDiffCommutative = property $ commutativity (<>) freeDiff

prop_FreeDiffInversion :: Property
prop_FreeDiffInversion = property $ inversion (<>) mempty invert freeDiff

prop_DiffOrd :: Property
prop_DiffOrd = property $ ord intDiff egte
  where
    egte (Sum x :-: Sum y) =
        ((:-:) `on` Sum) <$> Gen.int (Range.linear x 20) <*>
        Gen.int (Range.linear 0 y)

inversion :: (Show a, Eq a, Monad m) => (a -> a -> a) -> a -> (a -> a) -> Gen a -> PropertyT m ()
inversion (<+>) i inv gen = do
    xs <- forAll gen
    xs <+> inv xs === i
    inv xs <+> xs === i

main :: IO Bool
main = checkParallel $$discover
