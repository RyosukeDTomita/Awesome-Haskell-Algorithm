import Debug.Trace (traceShowId)

permutations' :: (Show a) => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs =
  [ x : p
  | (x, rest) <- selections xs,
    p <- permutations' rest -- 再帰でrestの中での並び替えを実施する
  ]
  where
    -- \| 先頭に来る要素を1つ選び、残りをそのままの順で返す
    -- [1, 2, 3]の場合 [ (1,[2,3]), (2,[1,3]), (3,[1,2]) ]
    selections :: (Show a) => [a] -> [(a, [a])]
    selections [] = []
    selections (y : ys) =
      -- traceShowId $ (y, ys)
      (y, ys)
        : [ (z, y : zs)
          | (z, zs) <- selections ys
          ]

main :: IO ()
main = do
  print $ permutations' [1, 2, 3]
