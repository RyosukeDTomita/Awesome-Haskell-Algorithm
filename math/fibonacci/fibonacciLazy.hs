{-# OPTIONS_GHC -Wno-x-partial #-}

-- | 遅延評価を活かしたフィボナッチ
fibs :: [Int]
-- [0, 1]は初期値にする
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- fibsにtail fibs(先頭を1つ落としたfibs)を足す。
-- ghci> 0 : 1 : zipWith (+) [0,1,1,2,3] [1,1,2,3]
-- [0,1,1,2,3,5]

main :: IO ()
main = do
  print $ take 36 fibs
