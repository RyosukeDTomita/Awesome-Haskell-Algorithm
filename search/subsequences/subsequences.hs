-- | 部分列
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

main :: IO ()
main = do
  print $ subsequences [1, 2, 3]
  print $ subsequences [1, 1, 2]
