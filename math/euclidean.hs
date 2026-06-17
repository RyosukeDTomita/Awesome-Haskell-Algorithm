-- | ユークリッドの互助法を使って最大公約数を求める
gcd' :: Int -> Int -> Int
gcd' a b
  | a < 0 || b < 0 = error "gcd': negative input"
  | a `mod` b == 0 = b
  | otherwise = gcd' b (a `mod` b)

-- | 最小公倍数を求める
lcm' :: Int -> Int -> Int
lcm' x y
  | x < 0 || y < 0 = error "lcm': negative input"
  | otherwise = x * y `quot` gcd' x y

-- quotは整数の切り捨て除算

main :: IO ()
main = do
  print $ gcd' 5 21 -- 1
  print $ gcd' 10 100 -- 10
  print $ lcm' 5 21 -- 105
  print $ lcm' 10 100 -- 100
