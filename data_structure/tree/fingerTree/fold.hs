{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import Data.Monoid (Sum (..))
import Prelude hiding (foldMap, mappend, mconcat)

-- | すでに要素がMonoidの列をそのまま畳み込む
fold :: (Foldable t, Monoid m) => t m -> m
fold xs = foldMap id xs

-- | monoid版fold
-- memptyは単位元
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f xs = foldr (\a acc -> mappend (f a) acc) mempty xs

-- | <>は2つの同じ型の値を一つにまとめる演算
mappend :: (Semigroup a) => a -> a -> a
mappend a b = a <> b

main :: IO ()
main = do
  let sumList = fold [1, 2, 3 :: Sum Int] -- リストがMonoidになるように型を付与
  print sumList -- Sum {getSum = 6}
  print $ foldMap Sum [1, 2, 3]
