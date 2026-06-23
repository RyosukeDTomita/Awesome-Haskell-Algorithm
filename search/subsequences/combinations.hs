-- | 組み合わせ(nCk)
-- n個のリストからk個を選ぶが、選んだ後の順序は問わない。
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]] -- k個選び終えたら空列を1つ返す
combinations _ [] = [] -- まだ選ぶ必要があるのに候補が尽きたら打ち切り k > nの時は0なので[]を返している。
combinations k (x : xs) =
  -- 先頭xを「選ぶ」場合と「選ばない」場合に分ける
  map (x :) (combinations (k - 1) xs) -- xを選び、残りからk-1個
    ++ combinations k xs -- xを選ばず、残りからk個

main :: IO ()
main = do
  print $ combinations 2 [1, 2, 3] -- [[1,2],[1,3],[2,3]]
  print $ combinations 2 [1, 1, 1] -- [[1,1],[1,1],[1,1]]
