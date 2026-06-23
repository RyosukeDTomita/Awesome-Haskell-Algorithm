{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -Wunused-imports #-}

import Data.List (inits, tails)

-- | 連続部分列(空を含む)
-- ghci> tails "abc"
-- ["abc","bc","c",""]
-- ghci> map inits $ tails "abc"
-- [["","a","ab","abc"],["","b","bc"],["","c"],[""]]
-- ghci> map (tail . inits) $ tails "abc"
-- [["a","ab","abc"],["b","bc"],["c"],[]]
-- ghci> concatMap (tail . inits) $ tails "abc"
-- ["a","ab","abc","b","bc","c"]
substrings :: [a] -> [[a]]
substrings s = [] : (concatMap (tail . inits) $ tails s)

main :: IO ()
main = do
  print $ substrings [1, 2, 3]
  print $ substrings [1, 1, 2]
