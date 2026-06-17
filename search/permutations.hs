permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs =
  [ x : p
  | (x, rest) <- selections xs,
    p <- permutations' rest
  ]
  where
    -- 各要素を1つ取り出し、(取り出した要素, 残り) を全通り返す
    selections :: [a] -> [(a, [a])]
    selections [] = []
    selections (y : ys) =
      (y, ys) : [(z, y : zs) | (z, zs) <- selections ys]

main :: IO ()
main = print $ permutations' [1, 2, 3 :: Int]
