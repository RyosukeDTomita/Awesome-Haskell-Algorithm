-- | 部分順列(nPk)
-- n個のリストからk個を選んで並べる。順序の違いは区別する。
partialPermutations :: Int -> [a] -> [[a]]
partialPermutations 0 _ = [[]] -- k個選び終えたら空列を1つ返す
partialPermutations _ [] = [] -- まだ選ぶ必要があるのに候補が尽きたら打ち切り
partialPermutations k xs =
  [ x : p
  | (x, rest) <- selections xs,
    p <- partialPermutations (k - 1) rest -- 残りからk-1個を選んで並べる
  ]
  where
    -- \| 先頭に来る要素を1つ選び、残りをそのままの順で返す
    -- [1, 2, 3]の場合 [ (1,[2,3]), (2,[1,3]), (3,[1,2]) ]
    selections :: [a] -> [(a, [a])]
    selections [] = []
    selections (y : ys) =
      (y, ys)
        : [ (z, y : zs)
          | (z, zs) <- selections ys
          ]

main :: IO ()
main = do
  print $ partialPermutations 2 [1, 2, 3]
  print $ partialPermutations 2 [1, 1, 2]
