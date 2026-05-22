-- Data.Set.Internal を踏襲した重み平衡二分探索木 (Adams weight-balanced tree)
-- Bin にサイズを格納するのは、平衡条件 (size l, size r) を O(1) で判定するため
data Set a
  = Tip -- 空
  | Bin Int a (Set a) (Set a) -- 部分木に含まれる総数 このツリーの要素 左の枝 右の枝
  deriving (Show)

-- 平衡定数 (Adams)
-- 平衡条件: max(size l, size r) <= delta * min(size l, size r)
-- 回転判定: size inner < ratio * size outer なら単回転、そうでなければ二重回転
delta, ratio :: Int
delta = 3
ratio = 2

----------------------------------------------------------------------
-- 基本操作
----------------------------------------------------------------------

empty :: Set a
empty = Tip -- コンストラクタを公開しない(隠蔽するために)空を表すものを作っている

singleton :: a -> Set a
singleton x = Bin 1 x Tip Tip

size :: Set a -> Int
size Tip           = 0
size (Bin n _ _ _) = n

member :: Ord a => a -> Set a -> Bool
member _ Tip = False
member x (Bin _ y l r)
  | x < y     = member x l
  | x > y     = member x r
  | otherwise = True

-- サイズを計算する Bin スマートコンストラクタ (平衡が崩れていないときに使う)
bin :: a -> Set a -> Set a -> Set a
bin x l r = Bin (1 + size l + size r) x l r

-- 挿入
add :: Ord a => a -> Set a -> Set a
add x Tip = singleton x
add x t@(Bin _ y l r)
  | x < y     = balance y (add x l) r
  | x > y     = balance y l (add x r)
  | otherwise = t

-- 削除
delete :: Ord a => a -> Set a -> Set a
delete _ Tip = Tip
delete x (Bin _ y l r)
  | x < y     = balance y (delete x l) r
  | x > y     = balance y l (delete x r)
  | otherwise = glue l r

-- 昇順リスト化 (差分蓄積で O(n))
members :: Set a -> [a]
members t = go t []
  where
    go Tip           acc = acc
    go (Bin _ x l r) acc = go l (x : go r acc)

----------------------------------------------------------------------
-- 平衡 (核心部分)
----------------------------------------------------------------------

-- 挿入/削除で1段ぶんサイズがずれた直後の平衡回復
balance :: a -> Set a -> Set a -> Set a
balance x l r
  | sl + sr <= 1    = Bin (sl + sr + 1) x l r
  | sr > delta * sl = rotateL x l r
  | sl > delta * sr = rotateR x l r
  | otherwise       = Bin (sl + sr + 1) x l r
  where
    sl = size l
    sr = size r

-- 右が重いときの回転選択
rotateL :: a -> Set a -> Set a -> Set a
rotateL x l r@(Bin _ _ rl rr)
  | size rl < ratio * size rr = singleL x l r
  | otherwise                 = doubleL x l r
rotateL _ _ Tip = error "rotateL: Tip" -- 不変条件より到達不能

-- 左が重いときの回転選択
rotateR :: a -> Set a -> Set a -> Set a
rotateR x l@(Bin _ _ ll lr) r
  | size lr < ratio * size ll = singleR x l r
  | otherwise                 = doubleR x l r
rotateR _ Tip _ = error "rotateR: Tip" -- 不変条件より到達不能

-- 4種の基本回転 (Tip ケースは不変条件より到達不能)
singleL, singleR, doubleL, doubleR :: a -> Set a -> Set a -> Set a
singleL x l (Bin _ y rl rr)                = bin y (bin x l rl) rr
singleL _ _ Tip                            = error "singleL: Tip"
singleR x (Bin _ y ll lr) r                = bin y ll (bin x lr r)
singleR _ Tip _                            = error "singleR: Tip"
doubleL x l (Bin _ y (Bin _ z rll rlr) rr) = bin z (bin x l rll) (bin y rlr rr)
doubleL _ _ _                              = error "doubleL: Tip"
doubleR x (Bin _ y ll (Bin _ z lrl lrr)) r = bin z (bin y ll lrl) (bin x lrr r)
doubleR _ _ _                              = error "doubleR: Tip"

----------------------------------------------------------------------
-- glue / link / split / minView / maxView
----------------------------------------------------------------------

-- delete 用: 重ならない 2 木を要素なしで結合
glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl x ll lr) r@(Bin sr y rl rr)
  | sl > sr   = let (m, l') = maxViewSure x ll lr in balance m l' r
  | otherwise = let (m, r') = minViewSure y rl rr in balance m l r'

-- 非空木から最小値と残りの木を取り出す
minViewSure :: a -> Set a -> Set a -> (a, Set a)
minViewSure x Tip r = (x, r)
minViewSure x (Bin _ y ll lr) r =
  let (m, l') = minViewSure y ll lr in (m, balance x l' r)

-- 非空木から最大値と残りの木を取り出す
maxViewSure :: a -> Set a -> Set a -> (a, Set a)
maxViewSure x l Tip = (x, l)
maxViewSure x l (Bin _ y rl rr) =
  let (m, r') = maxViewSure y rl rr in (m, balance x l r')

-- 任意のサイズ差を許して要素 a で連結 (union/intersection で使う)
-- 前提: l のすべての要素 < a < r のすべての要素
link :: Ord a => a -> Set a -> Set a -> Set a
link x Tip r = add x r
link x l Tip = add x l
link x l@(Bin sl y ll lr) r@(Bin sr z rl rr)
  | delta * sl < sr = balance z (link x l rl) rr
  | delta * sr < sl = balance y ll (link x lr r)
  | otherwise       = bin x l r

-- a 未満と a より大に二分割
split :: Ord a => a -> Set a -> (Set a, Set a)
split _ Tip = (Tip, Tip)
split x (Bin _ y l r)
  | x < y     = let (lt, gt) = split x l in (lt, link y gt r)
  | x > y     = let (lt, gt) = split x r in (link y l lt, gt)
  | otherwise = (l, r)

----------------------------------------------------------------------
-- 集合演算 (split/link/glue による対称的な再帰)
----------------------------------------------------------------------

-- 和集合
union :: Ord a => Set a -> Set a -> Set a
union Tip t2 = t2
union t1 Tip = t1
union (Bin _ x l1 r1) t2 =
  let (l2, r2) = split x t2
  in  link x (union l1 l2) (union r1 r2)

-- 差集合
difference :: Ord a => Set a -> Set a -> Set a
difference Tip _  = Tip
difference t1 Tip = t1
difference t1 (Bin _ x l2 r2) =
  let (l1, r1) = split x t1
  in  glue (difference l1 l2) (difference r1 r2)

-- 積集合
intersection :: Ord a => Set a -> Set a -> Set a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1 (Bin _ x l2 r2) =
  let (l1, r1) = split x t1
      lInt    = intersection l1 l2
      rInt    = intersection r1 r2
  in  if member x t1 then link x lInt rInt else glue lInt rInt

----------------------------------------------------------------------
-- 動作確認
----------------------------------------------------------------------

main :: IO ()
main = do
  let a = foldr add empty [5, 3, 8, 1, 4, 7, 9, 2, 6] :: Set Int
  let b = foldr add empty [4, 5, 6, 10, 11]           :: Set Int

  putStrLn "-- 構築 --"
  print (members a)                       -- [1..9]
  print (size a)                          -- 9

  putStrLn "-- singleton / empty --"
  print (members (singleton 'x'))         -- "x"
  print (size (empty :: Set Int))         -- 0

  putStrLn "-- member --"
  print (member 4 a, member 42 a)         -- (True,False)

  putStrLn "-- add / delete --"
  print (members (add 10 a))              -- [1..10]
  print (members (delete 5 a))            -- [1,2,3,4,6,7,8,9]

  putStrLn "-- union / difference / intersection --"
  print (members (a `union` b))           -- [1..11]
  print (members (a `difference` b))      -- [1,2,3,7,8,9]
  print (members (a `intersection` b))    -- [4,5,6]

  putStrLn "-- 平衡確認: 昇順挿入でも木が偏らない --"
  let sorted = foldr add empty [1..15] :: Set Int
  print sorted
