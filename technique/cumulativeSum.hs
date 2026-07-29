{-# OPTIONS_GHC -Wunused-imports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
import Data.Vector.Unboxed qualified as VU
import Prelude hiding (scanl)

-- | scanlを学習用にリライト
-- foldlが最終結果だけを返すのに対し、scanlは途中経過をすべて残す。
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ acc [] = [acc]
scanl f acc (x : xs) = acc : scanl f (f acc x) xs


-- | 累積和sから半開区間[l, r)の和を求める(0-index)
-- (もとのリストでいうと、x_lからx_(r-1)までの和)
-- l == rのときは空区間となり0を返す。
-- NOTE: 計算式自体は引き算1回だが、リストの(!!)が先頭からの走査でO(n)のため、クエリ全体ではO(r)かかってしまう。O(1)にしたい場合は下のVector版を使う。
rangeSum :: [Int] -> Int -> Int -> Int
rangeSum s l r = s !! r - s !! l

-- | Vector版rangeSum
-- VU.(!)は連続領域への添字アクセスなのでO(1)。
rangeSumVec :: VU.Vector Int -> Int -> Int -> Int
rangeSumVec s l r = s VU.! r - s VU.! l

main :: IO ()
main = do
  let xs = [3, 1, 4, 1, 5, 9, 2, 6]
  let s = scanl (+) 0 xs
  print s -- [0,3,4,8,9,14,23,25,31]
  print $ rangeSum s 0 8 -- 31: 全体の和
  print $ rangeSum s 2 5 -- 10: [4,1,5]の和
  print $ rangeSum s 0 1 -- 3: 先頭1要素だけ
  print $ rangeSum s 3 3 -- 0: 空区間

  -- Vector版。区間和クエリがO(1)になるので競技プログラミングはこちらを推奨。
  let sVec = VU.scanl' (+) 0 $ VU.fromList xs -- scanl'のほうがサンクが潰せるので効率がいい
  print sVec -- [0,3,4,8,9,14,23,25,31]
  print $ rangeSumVec sVec 0 8 -- 31
  print $ rangeSumVec sVec 2 5 -- 10
  print $ rangeSumVec sVec 0 1 -- 3
  print $ rangeSumVec sVec 3 3 -- 0
