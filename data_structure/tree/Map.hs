import Prelude hiding (lookup) -- 自前のlookupとPrelude.lookupの衝突を避ける

-- | Data.Map.Lazyを踏襲した重み平衡二分探索木(Adams weight-balanced tree)
-- Binにサイズを格納するのは、平衡条件(size l, size r)をO(1)で判定するため
-- 平衡の仕組みはSetと同じで、キーを使って平衡をとっている
data Map k a
 -- | 部分木に含まれる総数 このツリーのキー このツリーのvalue 左の枝 右の枝(右の枝の方が値が大きくなる)
  = Bin Size k a (Map k a) (Map k a)
  | Tip -- 空
  deriving (Show)

type Size = Int

-- | 平衡定数(Adams)
delta, ratio :: Int
delta = 3 -- 回転するかどうか
ratio = 2 -- 単回転 or 二重回転

empty :: Map k a
empty = Tip

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip

-- | Mapからサイズの値を取得するO(1)
-- 実際のData.Map.lazyではdata Map k a  = Bin {-# UNPACK #-} !Size !k a !(Map k a) !(Map k a)のようにunpack + strictでデータ型を作っている。
-- NOTE: unpackできるのは評価済みのフィールドだけである。unpackするとポインタを介さずに値にアクセスできるので間接参照が一段浅くなる。
size :: Map k a -> Int
size Tip = 0
size (Bin n _ _ _ _) = n

member :: (Ord k) => k -> Map k a -> Bool
member _ Tip = False
member k (Bin _ kx _ l r)
  | k < kx = member k l
  | k > kx = member k r
  | otherwise = True

-- | サイズを計算するBinスマートコンストラクタ(平衡が崩れていないときに使う)
bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (1 + size l + size r) k x l r

-- | キー昇順の(キー, 値)リスト化(差分蓄積でO(n))
members :: Map k a -> [(k, a)]
members t = go t []
  where
    go Tip acc = acc
    go (Bin _ k x l r) acc = go l ((k, x) : go r acc)

-- | 木を整形して標準出力に表示する(表示用なのでData.Mapの公式関数ではない。)
--
-- 各ノードは"キー:値"で表す。右の子を上・左の子を下に描く。こうすると出力を
-- 時計回りに90°回したとき左右の枝の位置が通常の木と一致する。
-- 片方だけTipのときは位置が分かるよう"·"を出す。
prettyPrint :: (Show k, Show a) => Map k a -> IO ()
prettyPrint t = putStr (render t)
  where
    render Tip = "(empty)\n"
    render s = go "" s

    go _ Tip = "·\n"
    go childPrefix (Bin _ k x l r) =
      show k ++ ":" ++ show x ++ "\n" ++ drawChildren childPrefix l r

    drawChildren _ Tip Tip = "" -- 葉は子を描かない
    drawChildren prefix l r =
      prefix
        ++ "├── "
        ++ go (prefix ++ "│   ") r
        ++ prefix
        ++ "└── "
        ++ go (prefix ++ "    ") l

-- | キーに対応する値を探す。見つからなければNothing。O(log n)
lookup :: (Ord k) => k -> Map k a -> Maybe a
lookup _ Tip = Nothing
lookup k (Bin _ kx x l r)
  | k < kx = lookup k l
  | k > kx = lookup k r
  | otherwise = Just x

-- | キーに対応する値を返す。見つからなければエラー(Data.Mapの(!)相当)。O(log n)
find :: (Ord k) => k -> Map k a -> a
find _ Tip = error "find: given key is not an element in the map"
find k (Bin _ kx x l r)
  | k < kx = find k l
  | k > kx = find k r
  | otherwise = x

-- | (キー, 値)のリストを左から順に挿入して構築する
-- 同じキーが複数あるときは後勝ち(insertが上書きするため)
fromList :: (Ord k) => [(k, a)] -> Map k a
fromList kxs = foldl (\m (k, x) -> insert k x m) empty kxs

-- | 挿入。同じキーが既にあれば値を上書きする
insert :: (Ord k) => k -> a -> Map k a -> Map k a
insert k x Tip = singleton k x
insert k x (Bin sz ky y l r)
  | k < ky = balance ky y (insert k x l) r
  | k > ky = balance ky y l (insert k x r)
  | otherwise = Bin sz k x l r

-- | 挿入。既存キーがあれば新値と旧値をfで合成する(f 新値 旧値)。なければそのまま挿入
insertWith :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith _f k x Tip = singleton k x
insertWith f k x (Bin sz ky y l r)
  | k < ky = balance ky y (insertWith f k x l) r
  | k > ky = balance ky y l (insertWith f k x r)
  | otherwise = Bin sz ky (f x y) l r -- 既存キーはサイズ不変なので生コンストラクタ

-- | insertWithKeyとlookupを一度に行う。旧値(あれば)と挿入後の木を返す
-- 合成関数はキーも受け取る(f キー 新値 旧値)。1回の降下で済むのが利点
insertLookupWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey _f k x Tip = (Nothing, singleton k x)
insertLookupWithKey f k x (Bin sz ky y l r)
  | k < ky = let (found, l') = insertLookupWithKey f k x l in (found, balance ky y l' r)
  | k > ky = let (found, r') = insertLookupWithKey f k x r in (found, balance ky y l r')
  | otherwise = (Just y, Bin sz ky (f ky x y) l r)

-- | 削除
delete :: (Ord k) => k -> Map k a -> Map k a
delete _ Tip = Tip
delete k (Bin _ ky y l r)
  | k < ky = balance ky y (delete k l) r
  | k > ky = balance ky y l (delete k r)
  | otherwise = glue l r

----------------------------------------------------------------------
-- 平衡(核心部分)
----------------------------------------------------------------------

-- | 挿入/削除で1段ぶんサイズがずれた直後の平衡回復
balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | sl + sr <= 1 = Bin (sl + sr + 1) k x l r -- サイズが既知の場合はスマートコンストラクタbinではなく、生コンストラクタを使う
  | sr > delta * sl = rotateL k x l r
  | sl > delta * sr = rotateR k x l r
  | otherwise = Bin (sl + sr + 1) k x l r
  where
    sl = size l -- NOTE: ノードから値を呼んでいるだけなのでO(1)。
    sr = size r

-- | 右が重いときの回転選択
rotateL :: k -> a -> Map k a -> Map k a -> Map k a
rotateL k x l r@(Bin _ _ _ rl rr)
  | size rl < ratio * size rr = singleL k x l r
  | otherwise = doubleL k x l r
rotateL _ _ _ Tip = error "rotateL: Tip" -- 不変条件より到達不能

-- | 左が重いときの回転選択
rotateR :: k -> a -> Map k a -> Map k a -> Map k a
rotateR k x l@(Bin _ _ _ ll lr) r
  | size lr < ratio * size ll = singleR k x l r
  | otherwise = doubleR k x l r
rotateR _ _ Tip _ = error "rotateR: Tip" -- 不変条件より到達不能

-- | 4種の基本回転(Tipケースは不変条件より到達不能)
-- 仕組みはSet.hsと同じ
singleL, singleR, doubleL, doubleR :: k -> a -> Map k a -> Map k a -> Map k a
singleL kx vx l (Bin _ ky vy rl rr) = bin ky vy (bin kx vx l rl) rr
singleL _ _ _ Tip = error "singleL: Tip"
singleR kx vx (Bin _ ky vy ll lr) r = bin ky vy ll (bin kx vx lr r)
singleR _ _ Tip _ = error "singleR: Tip"
doubleL kx vx l (Bin _ ky vy (Bin _ kz vz rll rlr) rr) = bin kz vz (bin kx vx l rll) (bin ky vy rlr rr)
doubleL _ _ _ _ = error "doubleL: Tip"
doubleR kx vx (Bin _ ky vy ll (Bin _ kz vz lrl lrr)) r = bin kz vz (bin ky vy ll lrl) (bin kx vx lrr r)
doubleR _ _ _ _ = error "doubleR: Tip"

-- | delete用: 重ならない2木を結合
glue :: Map k a -> Map k a -> Map k a
glue Tip r = r
glue l Tip = l
glue l@(Bin sl kx vx ll lr) r@(Bin sr ky vy rl rr) -- sl、srはsize l size r
  | sl > sr = let ((km, vm), l') = maxViewSure kx vx ll lr in balance km vm l' r
  | otherwise = let ((km, vm), r') = minViewSure ky vy rl rr in balance km vm l r'

-- | 非空木から最小キーの(キー, 値)と残りの木を取り出す
-- 左側の枝だけを掘っていく単一パス降下
minViewSure :: k -> a -> Map k a -> Map k a -> ((k, a), Map k a)
minViewSure k x Tip r = ((k, x), r) -- 左側のノードがない=自分が最小
minViewSure k x (Bin _ kl vl ll lr) r =
  let (m, l') = minViewSure kl vl ll lr
   in (m, balance k x l' r)

-- | 非空木から最大キーの(キー, 値)と残りの木を取り出す
-- 右側の枝だけを掘っていく単一パス降下
maxViewSure :: k -> a -> Map k a -> Map k a -> ((k, a), Map k a)
maxViewSure k x l Tip = ((k, x), l) -- 右側のノードがない=自分が最大
maxViewSure k x l (Bin _ kr vr rl rr) =
  let (m, r') = maxViewSure kr vr rl rr
   in (m, balance k x l r')

main :: IO ()
main = do
  putStrLn "-- 構築 (キー: 人名, 値: 年齢) --"
  let t =
        fromList
          [ ("John", 30),
            ("Taro", 25),
            ("Alice", 28),
            ("Bob", 22),
            ("Carol", 35),
            ("Dave", 40),
            ("Eve", 19),
            ("Frank", 33),
            ("Grace", 27)
          ] ::
          Map String Int
  print $ members t -- 名前の昇順に並ぶ
  print $ size t -- 9
  prettyPrint t
  putStrLn "-- singleton / empty --"
  print $ members $ singleton "Taro" (25 :: Int) -- [("Taro",25)]
  print $ size (empty :: Map String Int) -- 0
  putStrLn "-- member / lookup / find --"
  print $ member "Alice" t -- True
  print $ member "Zoe" t  -- False
  print $ lookup "Alice" t -- Just 28
  print $ lookup "Zoe" t -- Nothing
  print (find "Alice" t) -- 28
  putStrLn "-- insert (既存キーは上書き) --"
  let t' = insert "Alice" 99 t
  prettyPrint t'
  putStrLn "-- insertWith (既存キーは新旧の値を合成) --"
  let t''  = insertWith (+) "Alice" 1 t' -- Just 29 (28 + 1)
  let t''' = insertWith (+) "Zoe" 1 t'' -- Just 1 (新規)
  prettyPrint t'''
  putStrLn "-- delete --"
  let t'''' = delete "John" t'''
  print (members t'''') -- Johnが消える
  prettyPrint t''''

  putStrLn "-- insertLookupWithKey (旧値の取得と挿入を同時に) --"
  let (age, t''''') = insertLookupWithKey (\_ _ old -> old + 1) "Alice" 1 t''''
  print age
  prettyPrint t'''''

  putStrLn "-- 単語カウント --"
  -- 各単語をキーに、出現回数を値にする。既出ならinsertWith (+)で+1する
  let text = "the quick brown fox the lazy dog the quick fox"
      wordCount = foldl (\m w -> insertWith (+) w 1 m) empty (words text) :: Map String Int
  print $ members wordCount -- [("brown",1),("dog",1),("fox",2),("lazy",1),("quick",2),("the",3)]
