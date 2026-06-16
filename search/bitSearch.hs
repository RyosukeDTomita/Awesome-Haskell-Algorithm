import Data.List (reverse, unfoldr)

-- | 2進数変換して表示
toBinary :: Int -> String
toBinary 0 = "0"
toBinary n = reverse $ unfoldr step n
  where
    step :: Int -> Maybe (Char, Int)
    step 0 = Nothing
    step n =
      let (q, r) = n `divMod` 2
       in Just (if r == 0 then '0' else '1', q)

-- | xをnビット左シフトする。x * 2^nと同じで、bit列を上位方向へずらす。
shiftL' :: Int -> Int -> Int
shiftL' x n = x * (2 ^ n)

-- | xのiビット目(0始まり)が1かどうかを判定する。
-- iビット右に寄せて最下位ビットの偶奇を見ることで判定する。
testBit' :: Int -> Int -> Bool
testBit' x i = (x `div` (2 ^ i)) `mod` 2 == 1

-- | n個の要素を持つ集合の部分集合をbit列(0..2^n-1)で全列挙する。
subSets :: [a] -> [[a]]
subSets xs =
  [
    [
      x
      | (i, x) <- zip [0 ..] xs,
      testBit' bit i
    ]
  | bit <- [0 .. (1 `shiftL'` n) - 1] -- n = 3なら bit <- [0..7]
  ]
  where
    n = length xs

-- | 部分集合の総和がtargetに一致するものを数えるbit全探索の例。
countSubsetSum :: Int -> [Int] -> Int
countSubsetSum target xs = length [
    s
    | s <- subSets xs,
    sum s == target
  ]

main :: IO ()
main = do
  print "-----shiftL-----"
  let x = 3
  print $ toBinary 3
  print $ toBinary $ shiftL' x 1
  print $ shiftL' x 1
  print $ toBinary $ shiftL' x 2
  print $ shiftL' x 2

  print "-----testBit-----"
  -- []
  print $ testBit' 0 0
  print $ testBit' 0 1
  print $ testBit' 0 2
  -- [2,3]
  print $ testBit' 6 0
  print $ testBit' 6 1
  print $ testBit' 6 2
  -- [1,2,3]
  print $ testBit' 7 0
  print $ testBit' 7 1
  print $ testBit' 7 2

  print "-----subSets-----"
  let xs = [1, 2, 3]
  -- 2^3 = 8通りの部分集合を全列挙
  print $ subSets xs

  print "-----bit search-----"
  -- 総和が3になる部分集合は {3}, {1,2} の2通り
  print $ countSubsetSum 3 xs
