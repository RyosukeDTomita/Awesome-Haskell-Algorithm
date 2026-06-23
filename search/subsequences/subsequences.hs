{-# OPTIONS_GHC -Wunused-imports #-}

import Data.Bits (shiftL, testBit)

-- | 部分列(再帰版)
-- 先頭要素を入れる/入れないで分岐して再帰する。
-- 遅延評価が効くため、無限リストや先頭だけ取り出す用途にも使える。
subsequences :: [a] -> [[a]]
subsequences xs = [] : nonEmptySubsequences xs

-- | 部分列の空以外の部分を求める
-- [1, 2, 3]の場合
-- [1] : foldr f [] (nonEmptySubsequences [2,3])
-- = [1] : foldr f [] ([2] : foldr f [] (nonEmptySubsequences [3]))
-- = [1] : foldr f [] ([2] : foldr f [] ([3] : foldr f [] (nonEmptySubsequences []))
-- = [1] : foldr f [] ([2] : foldr f [] ([3] : foldr f [] []))
-- = [1] : foldr f [] ([2] : foldr f [] ([3] : []))
-- = [1] : foldr f [] ([2] : foldr f [] [[3]])
-- = [1] : foldr f [] ([2] : [[3], [2, 3]])
-- = [1] : foldr f [] ([[2], [3], [2, 3]])
-- = [1] : [[2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3]]
-- = [[1], [2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3]]
nonEmptySubsequences :: forall a. [a] -> [[a]]
nonEmptySubsequences [] = []
nonEmptySubsequences (x : xs) = [x] : foldr f [] (nonEmptySubsequences xs)
  where
    -- x=2の時、 foldr f [] [[3]]
    -- = [3] : (2 : [3]) : []
    -- = [3] : ([2, 3]) : []
    -- [[3], [2, 3]]
    f :: [a] -> [[a]] -> [[a]]
    f ys acc = ys : (x : ys) : acc

-- | 部分列(bit全探索版)
-- 各要素を「入れる/入れない」をnビットで表し、0 .. 2^n-1のマスクを全列挙する。
-- maskの第iビットがxsの第i要素の採用に対応する。
-- 集合を整数1個で表現できるのが強みだが、先頭でlength xsを評価するため有限リスト限定。
-- 列挙順は再帰版のsubsequencesと一致する。
subsequences' :: [a] -> [[a]]
subsequences' xs =
  [ [x | (i, x) <- zip [0 ..] xs, testBit mask i]
  | mask <- [0 .. (1 `shiftL` n) - 1]
  ]
  where
    n = length xs

main :: IO ()
main = do
  print $ subsequences [1, 2, 3]
  print $ subsequences [1, 1, 2]
  print $ subsequences' [1, 2, 3]
  print $ subsequences' [1, 1, 2]
