-- 円周率piが3.14以上であることがわかるn角形をもとめる(多角形近似)
import Text.Printf (printf)

-- n角形によるpiの近似
-- 半径 1 の円の円周は2 * pi
-- 内接正 n 角形の周長はn * 2 * sin(pi / n)
-- 2 * pi ≒ n * 2 * sin(pi / n)
-- 両辺を 2 で割ると、
-- pi ≒ n * sin(pi / n)
approxPi :: Int -> Double
approxPi n
  | n >= 3 = fromIntegral n * sin (pi / fromIntegral n)
  | otherwise = error "invalid n"

iteratePi :: Int -> Int
iteratePi n
  | approxPi n >= 3.14 = n
  | otherwise = iteratePi (n + 1)

main :: IO ()
main = do
  let initialN = 3
  print $ iteratePi initialN
