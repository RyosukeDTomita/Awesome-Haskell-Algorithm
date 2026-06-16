-- | 今回の状態と次回の状態を引き回してフィボナッチ数列作成
fibSeq :: Int -> [Int]
fibSeq n = map fst $ take n $ iterate (\(cur, next) -> (next, cur + next)) (0, 1)

main :: IO ()
main = do
  print $ fibSeq 1
  print $ fibSeq 2
  print $ fibSeq 36
