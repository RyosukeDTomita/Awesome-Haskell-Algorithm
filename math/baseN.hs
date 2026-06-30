{-# LANGUAGE BangPatterns #-}

import Data.Char (digitToInt, intToDigit)
import Prelude hiding (foldl')

-- unfoldrを学習用にリライト
unfoldr :: forall a b. (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b0 = go b0
  where
    go :: b -> [a]
    go b = case f b of
      Just (a, new_b) -> a : go new_b
      Nothing -> []

-- | foldl'を学習用にリライト
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ !acc [] = acc -- 累積値
foldl' f !acc (x : xs) = foldl' f (f acc x) xs

-- 10進数をn進数に変換する
toBaseN :: Int -> Int -> String
toBaseN _ 0 = "0"
toBaseN n x = reverse $ unfoldr go x
  where
    go :: Int -> Maybe (Char, Int)
    go 0 = Nothing
    go y =
      let (q, r) = y `divMod` n
       in Just (intToDigit r, q)

-- n進数を10進数に変換する
fromBaseN :: Int -> String -> Int
fromBaseN n xStr = foldl' (\acc c -> acc * n + digitToInt c) 0 $ xStr

main :: IO ()
main = do
  -- 2進法変換
  print $ toBaseN 2 8 -- 1000
  print $ toBaseN 2 7 -- 111
  -- 7進数変換
  print $ toBaseN 7 100 -- 202

  -- 2進数から戻す
  print $ fromBaseN 2 "1000" -- 8
  print $ fromBaseN 2 "111" -- 7
  -- 7進数から戻す
  print $ fromBaseN 7 "202" -- 100
