import qualified Data.Vector as V
import Data.List (sort)


-- | targetを二分探索で探す
-- NOTE: 二分探索の各ステップでインデックスへのアクセスが必要であり、リストの`!!`はリストのデータ構造上O(n)であるため、O(1)でランダムアクセスできるData.Vectorを使用している。
binarySearch :: V.Vector Int -> Int -> Int -> Int -> Bool
binarySearch xs target left right
  | left > right = False -- 要素が見つからない
  | xs V.! mid == target = True
  | xs V.! mid < target = binarySearch xs target (mid + 1) right
  | otherwise = binarySearch xs target left (mid - 1)
  where
    mid = (left + right) `div` 2

main :: IO ()
main = do
  let randomList = [30, 75, 69, 16, 47, 77, 60, 80, 74, 8, 77, 1, 60, 33, 70, 29, 24, 91, 60, 69]
  let sortedVec = V.fromList (sort randomList) -- 配列がsort済みである必要がある
  print sortedVec
  print $ binarySearch sortedVec 47 0 (V.length sortedVec - 1) -- 発見
  print $ binarySearch sortedVec 100 0 (V.length sortedVec - 1) -- 発見できず
