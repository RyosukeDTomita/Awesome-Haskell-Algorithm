{-# OPTIONS_GHC -Wunused-imports #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

import Data.List (inits, tails)

-- | 連続部分列(空を含む)
substrings :: [a] -> [[a]]
substrings s = [] : (concatMap (tail . inits) $ tails s)


main :: IO ()
main = do
  print $ substrings [1, 2, 3]
  print $ substrings [1, 1, 2]
