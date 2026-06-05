-- | Data.Set.Internalを踏襲した重み平衡二分探索木(Adams weight-balanced tree)
-- Binにサイズを格納するのは、平衡条件(size l, size r)をO(1)で判定するため
data Set a
  = -- | 空
    Tip
  | -- | 部分木に含まれる総数 このツリーの要素 左の枝 右の枝
    Bin Int a (Set a) (Set a)
  deriving (Show)

-- | 平衡定数(Adams)
delta, ratio :: Int
delta = 3 -- 回転するかどうか
ratio = 2 -- 単回転 or 二重回転

empty :: Set a
empty = Tip -- コンストラクタを公開しない(隠蔽するために)空を表すものを作っている

singleton :: a -> Set a
singleton x = Bin 1 x Tip Tip

-- | Setからサイズの値を読みとるO(1)。
-- 実際のData.Setではdata Set a = Bin {-# UNPACK #-} !Size !a !(Set a) !(Set a)のようにunpack + strictでデータ型を作っている。
-- NOTE: unpackできるのは評価済みのフィールドだけである。unpackするとポインタを介さずに値にアクセスできるので間接参照が一段浅くなる。
size :: Set a -> Int
size Tip = 0
size (Bin n _ _ _) = n

member :: (Ord a) => a -> Set a -> Bool
member _ Tip = False
member x (Bin _ y l r)
  | x < y = member x l
  | x > y = member x r
  | otherwise = True

-- | サイズを計算するBinスマートコンストラクタ(平衡が崩れていないときに使う)
bin :: a -> Set a -> Set a -> Set a
bin x l r = Bin (1 + size l + size r) x l r

-- | 昇順リスト化(差分蓄積でO(n))
members :: Set a -> [a]
members t = go t []
  where
    go Tip acc = acc
    go (Bin _ x l r) acc = go l (x : go r acc)

-- | リストの要素を左から順に挿入して構築する
fromList :: (Ord a) => [a] -> Set a
fromList xs = foldl (flip insert) empty xs

-- | 木を整形して標準出力に表示する(表示用なのでData.Setの公式関数ではない。)
--
-- 右の子を上・左の子を下に描く。こうすると出力を時計回りに90°回したとき
-- 左右の枝の位置が通常の木と一致する。片方だけTipのときは位置が分かるよう"·"を出す。
-- printだと枝文字が文字化けするため、ユーザが使い方を間違えないように、putStrまで内包している。
prettyPrint :: (Show a) => Set a -> IO ()
prettyPrint t = putStr (render t)
  where
    render Tip = "(empty)\n"
    render s = go "" s

    go _ Tip = "·\n"
    go childPrefix (Bin _ x l r) =
      show x ++ "\n" ++ drawChildren childPrefix l r

    drawChildren _ Tip Tip = "" -- 葉は子を描かない
    drawChildren prefix l r =
      prefix
        ++ "├── "
        ++ go (prefix ++ "│   ") r
        ++ prefix
        ++ "└── "
        ++ go (prefix ++ "    ") l

-- | 挿入
insert :: (Ord a) => a -> Set a -> Set a
insert x Tip = singleton x
insert x t@(Bin _ y l r)
  | x < y = balance y (insert x l) r
  | x > y = balance y l (insert x r)
  | otherwise = t

-- | 削除
delete :: (Ord a) => a -> Set a -> Set a
delete _ Tip = Tip
delete x (Bin _ y l r)
  | x < y = balance y (delete x l) r
  | x > y = balance y l (delete x r)
  | otherwise = glue l r

----------------------------------------------------------------------
-- 平衡(核心部分)
----------------------------------------------------------------------

-- | 挿入/削除で1段ぶんサイズがずれた直後の平衡回復
balance :: a -> Set a -> Set a -> Set a
balance x l r
  | sl + sr <= 1 = Bin (sl + sr + 1) x l r -- サイズが既知の場合はスマートコンストラクタbinではなく、生コンストラクタを使う
  | sr > delta * sl = rotateL x l r
  | sl > delta * sr = rotateR x l r
  | otherwise = Bin (sl + sr + 1) x l r
  where
    sl = size l -- NOTE: ノードから値を呼んでいるだけなのでO(1)。
    sr = size r

-- | 右が重いときの回転選択
rotateL :: a -> Set a -> Set a -> Set a
rotateL x l r@(Bin _ _ rl rr)
  | size rl < ratio * size rr = singleL x l r
  | otherwise = doubleL x l r
rotateL _ _ Tip = error "rotateL: Tip" -- 不変条件より到達不能

-- | 左が重いときの回転選択
rotateR :: a -> Set a -> Set a -> Set a
rotateR x l@(Bin _ _ ll lr) r
  | size lr < ratio * size ll = singleR x l r
  | otherwise = doubleR x l r
rotateR _ Tip _ = error "rotateR: Tip" -- 不変条件より到達不能

-- | 4種の基本回転(Tipケースは不変条件より到達不能)
--
-- singleL:
--
-- >   回転前                       回転後
-- >      x                          y
-- >     / \                        / \
-- >    l   y   ← これが r          x   rr
-- >       / \                    / \
-- >      rl  rr                 l   rl
--
-- doubleL:
--
-- >   回転前                          回転後
-- >      x                              z
-- >     / \                          /     \
-- >    l   y    ← r                 x        y
-- >       / \                      / \      / \
-- > rl-> z   rr                   l  rll  rlr  rr
-- >     / \
-- >   rll  rlr
singleL, singleR, doubleL, doubleR :: a -> Set a -> Set a -> Set a
singleL x l (Bin _ y rl rr) = bin y (bin x l rl) rr
singleL _ _ Tip = error "singleL: Tip"
singleR x (Bin _ y ll lr) r = bin y ll (bin x lr r)
singleR _ Tip _ = error "singleR: Tip"
doubleL x l (Bin _ y (Bin _ z rll rlr) rr) = bin z (bin x l rll) (bin y rlr rr)
doubleL _ _ _ = error "doubleL: Tip"
doubleR x (Bin _ y ll (Bin _ z lrl lrr)) r = bin z (bin y ll lrl) (bin x lrr r)
doubleR _ _ _ = error "doubleR: Tip"

-- | delete用: 重ならない2木を要素なしで結合
glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl x ll lr) r@(Bin sr y rl rr) -- sl、srはsize l size r
  | sl > sr = let (m, l') = maxViewSure x ll lr in balance m l' r
  | otherwise = let (m, r') = minViewSure y rl rr in balance m l r'

-- | 非空木から最小値と残りの木を取り出す
-- 左側の枝だけを掘っていく単一パス降下
minViewSure :: a -> Set a -> Set a -> (a, Set a)
minViewSure x Tip r = (x, r) -- 左側のノードがない=自分が最小
minViewSure x (Bin _ y ll lr) r =
  let (m, l') = minViewSure y ll lr
   in (m, balance x l' r)

-- | 非空木から最大値と残りの木を取り出す
-- 右側の枝だけを掘っていく単一パス降下
maxViewSure :: a -> Set a -> Set a -> (a, Set a)
maxViewSure x l Tip = (x, l) -- 右側のノードがない=自分が最大
maxViewSure x l (Bin _ y rl rr) =
  let (m, r') = maxViewSure y rl rr
   in (m, balance x l r')

main :: IO ()
main = do
  putStrLn "-- 構築 --"
  let t = foldr insert empty [5, 3, 8, 1, 4, 7, 9, 2, 6] :: Set Int
  print (members t) -- [1..9]
  print (size t) -- 9
  prettyPrint t
  putStrLn "-- singleton / empty --"
  print (members (singleton 'x')) -- "x"
  print (size (empty :: Set Int)) -- 0
  putStrLn "-- member --"
  print (member 4 t, member 42 t) -- (True,False)
  putStrLn "-- insert / delete --"
  let t' = insert 10 t
  print (members t') -- [1..10]
  prettyPrint t'
  let t'' = delete 5 t
  print (members t'') -- [1,2,3,4,6,7,8,9]
  prettyPrint t''
  putStrLn "-- 平衡確認: 昇順挿入でも木が偏らない --"
  let sorted = foldr insert empty [1 .. 15] :: Set Int
  print sorted
  prettyPrint sorted

  putStrLn "\n=== 回転デモ (左右どちらでも仕組みは同じ。ここは右が重くなる L 系) ==="

  putStrLn "-- 単回転 (insert): [1,2] に 3 を追加 → 一直線型なので singleL --"
  let i1 = fromList [1, 2] :: Set Int
  prettyPrint i1
  putStrLn "↓ insert 3"
  prettyPrint (insert 3 i1)

  putStrLn "-- 二重回転 (insert): [1,3] に 2 を追加 → くの字型なので doubleL --"
  let i2 = fromList [1, 3] :: Set Int
  prettyPrint i2
  putStrLn "↓ insert 2"
  prettyPrint (insert 2 i2)

  putStrLn "-- 単回転 (delete): [2,1,3,4] から 1 を削除 → 右が重くなり singleL --"
  let d1 = fromList [2, 1, 3, 4] :: Set Int
  prettyPrint d1
  putStrLn "↓ delete 1"
  prettyPrint (delete 1 d1)

  putStrLn "-- 二重回転 (delete): [2,1,4,3] から 1 を削除 → 右が左重なので doubleL --"
  let d2 = fromList [2, 1, 4, 3] :: Set Int
  prettyPrint d2
  putStrLn "↓ delete 1"
  prettyPrint (delete 1 d2)
