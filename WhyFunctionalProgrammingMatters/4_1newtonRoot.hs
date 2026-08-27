-- https://www.sampou.org/haskell/article/whyfp.html

-- | 近似を1つ進める関数
-- nの平方根を求めるためはf x = x^2 - nにおいてf x == 0となるxの値を求めれば良い。
-- そこで、点A(a,a^2 - n)を通る接線を求めるとこれはy = 2ax - a^2 - nになり、これのx軸との交点のx座標はy=0とすると
-- 2ax = a^2 - n
-- x = (a^2 - n) / 2 a
next :: Double -> Double -> Double
next n x = (x + n / x) / 2

-- | 許容誤差よりも差の小さい2つの連続する近似値がでるまでiterateの結果をめくる
within :: Double -> [Double] -> Double
within eps (a : b : rest)
  | abs (a - b) <= eps = b -- 絶対誤差
  | otherwise = within eps (b : rest) -- aを捨てて再帰を住める

-- withinを使って平方根をもとめる例
sqrt' :: Double -> Double -> Double -> Double
sqrt' a0 eps n = within eps $ iterate (next n) a0

-- | ２つの近似値の比が1に近づくように、許容誤差よりも小さい２つの連続する近似値がでるまでiterateの結果をめくる
relative :: Double -> [Double] -> Double
relative eps (a : b : rest)
  | abs (a - b) <= eps * abs b = b -- abs (a - b) / abs b <= epsを変形したもの。いわゆる相対誤差
  | otherwise = relative eps (b : rest)

-- | relativeを使って平方根を求める例
relativesqrt :: Double -> Double -> Double -> Double
relativesqrt a0 eps n = relative eps $ iterate (next n) a0

main :: IO ()
main = do
  let n = fromIntegral 3 -- 平方根を求めたい値
  let a0 = fromInteger 1 -- 計算の初期値
  let eps = 0.001
  let epsRelative = 0.001
  print $ take 10 $ iterate (next n) 1
  print $ sqrt' a0 eps n
  print $ relativesqrt a0 epsRelative n
