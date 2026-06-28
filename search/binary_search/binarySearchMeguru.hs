{-# OPTIONS_GHC -Wunused-imports #-}

import Data.List (sort)
import Data.Vector.Unboxed qualified as VU

-- | めぐる式二分探索。target以上が最初に現れるindexを返す
-- 探索範囲を「条件を満たさない側ng」と「条件を満たす側ok」の2つのindexで保持し、
-- abs(ok - ng) == 1 になるまで範囲を狭めて、条件を満たす境界okを返す。
-- ng = -1(条件を満たさない側)、ok = length(条件を満たす側)で初期化する。
lowerBound :: VU.Vector Int -> Int -> Int
lowerBound xs target = go (-1) $ VU.length xs
  where
    go ng ok
      | abs (ok - ng) <= 1 = ok -- 条件を満たす側の境界okが答え
      | xs VU.! mid >= target = go ng mid -- midが条件を満たすのでok側を狭める
      | otherwise = go mid ok -- midが条件を満たさないのでng側を狭める
      where
        mid = (ng + ok) `div` 2

-- | lower_boundで求めたindexの位置にtargetがあるかで存在判定する。
-- めぐる式自体は境界のindexを返すだけなので、等値判定は探索の後で1回行う。
binarySearch :: VU.Vector Int -> Int -> Bool
binarySearch xs target = i < VU.length xs && xs VU.! i == target -- 求めたiが適切な範囲にあるか確認する。
  where
    i = lowerBound xs target

main :: IO ()
main = do
  let randomList = [30, 75, 69, 16, 47, 77, 60, 80, 74, 8, 77, 1, 60, 33, 70, 29, 24, 91, 60, 69]
  let sortedVec = VU.fromList $ sort randomList -- ソート済みである必要がある
  print sortedVec
  print $ binarySearch sortedVec 47 -- 発見
  print $ binarySearch sortedVec 100 -- 発見できず
