-- 双方向連結リスト(Doubly Linked List)
-- 添字ベースのprev/next配列で表現する。添字0を先頭番兵、添字(capacity+1)を末尾番兵とする。
-- 値本体はvalueArrに保持する。
-- 前後のノードの添字を書き換えるだけなので、挿入・削除は挿入/削除位置さえ分かればO(1)で行える
{-# OPTIONS_GHC -Wunused-imports #-}

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed.Mutable qualified as VUM

-- | dlValue: 値を格納する。
-- dlPrev: 前ノードの添字
-- dlNext: 次ノードの添字
data DList s = DList
  { dlValue :: VUM.STVector s Int,
    dlPrev :: VUM.STVector s Int,
    dlNext :: VUM.STVector s Int
  }

-- | 双方向連結リストのcapacityを決めてメモリを確保する
newDList :: Int -> ST s (DList s)
newDList capacity = do
  value <- VUM.new (capacity + 2)
  prev <- VUM.new (capacity + 2)
  next <- VUM.new (capacity + 2)
  VUM.write next 0 (capacity + 1)
  VUM.write prev (capacity + 1) 0
  return $ DList value prev next

-- | 添字afterの直後に、添字newIdxのノードを値valとして挿入するO(1)
-- afterとnewIdxがすでにつながっている状態では無限ループになる
-- e.g. dlNext 2 = 3、dlPrev 3 = 2 となっている時、after=2、newIdx=3で呼び出すと
-- nx <- VUM.read (dlNext dlist) 2 = 3となり、
-- VUM.write (dlNext dlist) 3 3となり、無限ループになる
-- NOTE: Haskellのレコードはフィールド名が取り出し関数になる
-- dlValue dlistでdlValueが取り出せる
insertAfter :: DList s -> Int -> Int -> Int -> ST s ()
insertAfter dlist after newIdx val = do
  nx <- VUM.read (dlNext dlist) after -- 追加される要素の次のindex
  VUM.write (dlValue dlist) newIdx val -- 値更新
  VUM.write (dlNext dlist) after newIdx
  VUM.write (dlPrev dlist) newIdx after
  VUM.write (dlNext dlist) newIdx nx
  VUM.write (dlPrev dlist) nx newIdx

-- | 添字idxのノードを削除するO(1)
-- DListから要素を削除する場合、dlPrev、dlNextの添字を更新するだけでdlValueの要素は削除しない。
-- なぜなら、dlValueの値を削除するとindexがずれてしまい、再構築が必要になるから。
-- dlValueの値を-999などで初期化しても問題ないが、今回はやらない。
deleteNode :: DList s -> Int -> ST s ()
deleteNode dlist idx = do
  p <- VUM.read (dlPrev dlist) idx
  nx <- VUM.read (dlNext dlist) idx
  VUM.write (dlNext dlist) p nx
  VUM.write (dlPrev dlist) nx p

-- | 先頭番兵(0)からnextを辿って正順にリスト化するO(n)
toList :: DList s -> ST s [Int]
toList dlist = go =<< VUM.read (dlNext dlist) 0
  where
    tailSentinel = VUM.length (dlNext dlist) - 1
    go i
      | i == tailSentinel = return []
      | otherwise = do
          v <- VUM.read (dlValue dlist) i
          rest <- go =<< VUM.read (dlNext dlist) i
          return (v : rest)

-- STモナドはIOを使えないので、returnしたものをまとめてprintしている。
main :: IO ()
main = print $ runST $ do
  dlist <- newDList 5

  -- 添字1..5に値11,22,33,44,55を順に挿入して [11,22,33,44,55] を作る
  forM_ (zip [1 ..] [11, 22, 30, 44, 55]) $ \(i, v) -> insertAfter dlist (i - 1) i v
  forward1 <- toList dlist

  -- 添字3(値33)を削除 -> [11,22,44,55]
  deleteNode dlist 3
  forward2 <- toList dlist

  -- 空いた添字3を再利用し、添字2の直後に値33を挿入 -> [11,22,20,44,55]
  insertAfter dlist 2 3 33
  forward3 <- toList dlist

  -- 値更新はinsertAfterではできないので、VUM.writeで直接更新
  VUM.write (dlValue dlist) 3 34
  forward4 <- toList dlist

  return (forward1, forward2, forward3, forward4)
