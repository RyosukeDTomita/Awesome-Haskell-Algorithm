fibSeq :: Int -> [Int]
-- (cur, next) = (F(n), F(n+1)) を持ち越し、次は (next, cur + next) = (F(n+1), F(n+2))
fibSeq n = map fst $ take n $ iterate (\(cur, next) -> (next, cur + next)) (0, 1)

main :: IO ()
main = do
  print $ fibSeq 1
  print $ fibSeq 2
  print $ fibSeq 36
