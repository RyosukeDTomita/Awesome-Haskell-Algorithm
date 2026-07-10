{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (span)

-- リストの同じ要素を連結して返す。
-- "ABBCCC"の場合
-- group "ABBCCC"
-- = group ('A' : "BBCCC")
-- = ('A' : ys) : group zs                    -- (ys, zs) = go 'A' "BBCCC"
-- = ('A' : []) : group "BBCCC"               -- 'A' /= 'B' なので go 'A' "BBCCC" = ([], "BBCCC")
-- = "A" : group "BBCCC"
-- = "A" : (('B' : ys) : group zs)            -- (ys, zs) = go 'B' "BCCC"
-- = "A" : (('B' : "B") : group "CCC")        -- go 'B' "BCCC" = ("B", "CCC")
-- = "A" : "BB" : group "CCC"
-- = "A" : "BB" : (('C' : ys) : group zs)     -- (ys, zs) = go 'C' "CC"
-- = "A" : "BB" : (('C' : "CC") : group "")   -- go 'C' "CC" = ("CC", "")
-- = "A" : "BB" : "CCC" : group ""
-- = "A" : "BB" : "CCC" : []
-- = ["A", "BB", "CCC"]
group :: forall a. (Eq a) => [a] -> [[a]]
group [] = []
group (x : xs) = (x : ys) : group zs
  where
    (ys, zs) = go x xs
    -- \| 同じ要素がでなくなるまで取得し、そこでリストを分割する
    -- NOTE: span (==x) xsを手書きしたもの
    -- span (==1) [1,2,3]
    -- ([1],[2,3])
    -- span (==2) [1,2,3]
    -- ([], [1,2,3])
    go :: a -> [a] -> ([a], [a])
    go _ [] = ([], [])
    go c ds@(d : ds')
      | c == d =
          let (ys', zs') = go c ds'
           in (d : ys', zs')
      | otherwise = ([], ds)

-- | ランレングス圧縮: 連続して現れる要素を(要素, 出現回数)のペアに変換する。
rle :: (Eq a) => [a] -> [(a, Int)]
rle xs = map toPair . group $ xs
  where
    toPair xs@(x : _) = (x, length xs)
    toPair [] = error "unreachable: group never returns an empty group"

main :: IO ()
main = do
  print $ rle "ABBCCC" -- [('A',1),('B',2),('C',3)]
  print $ rle "ABBCCCA" -- [('A',1),('B',2),('C',3),('A',1)]
