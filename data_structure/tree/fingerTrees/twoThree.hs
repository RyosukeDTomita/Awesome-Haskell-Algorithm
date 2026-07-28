{-# LANGUAGE MonoLocalBinds #-}
{-# OPTIONS_GHC -Wunused-imports #-}

-- | 入れ子データ型(非正則再帰)による完全平衡2-3木。
-- 要素0の木は表現できない。
-- 要素1の木はZero、要素2、要素3の木はNode2とNode3で表現できるのでこれらの組み合わせでどのサイズの木も表現できる。
-- すべての葉が同じ高さに並ぶ
data Tree a
  = Zero a -- 高さ0。要素をちょうど1つ持つ
  | Succ (Tree (Node a)) -- 1段深い木。要素はNodeで束ねられている
  deriving (Show)

-- | 2-3木の内部ノード。子を2つ or 3つ持つ
data Node a
  = Node2 a a
  | Node3 a a a
  deriving (Show)

-- | 木を昇順(左から右)の要素リストに変換する。
toList :: Tree a -> [a]
toList (Zero a) = [a]
toList (Succ t) = concatMap nodeToList (toList t)
  where
    -- \| Nodeを要素のリストに展開する
    nodeToList :: Node a -> [a]
    nodeToList (Node2 a b) = [a, b]
    nodeToList (Node3 a b c) = [a, b, c]

-- | 木の高さ(Succの個数)
depth :: Tree a -> Int
depth (Zero _) = 0
depth (Succ t) = 1 + depth t

-- | 木が持つ要素数
size :: Tree a -> Int
size t = length $ toList t

-- | 木のデータをまとめて出力する
printTree :: Tree Int -> IO ()
printTree t = do
  putStrLn $ "tree : " ++ show t
  putStrLn $ "depth: " ++ show (depth t)
  putStrLn $ "size : " ++ show (size t)
  putStrLn $ "list : " ++ show (toList t)
  putStr $ "shape:\n" ++ renderRose (treeToRose t)

-- | (アルゴリズムの本質部分ではない) 木を字下げ付きで描画するための中間表現。ラベルと子のリストを持つ多分木
data Rose = Rose String [Rose]

-- | ネストした要素型(a, Node a, Node (Node a), ...)を共通にRoseへ変換する。
class ToRose a where
  toRose :: a -> Rose

instance ToRose Int where
  toRose x = Rose (show x) []

instance (ToRose a) => ToRose (Node a) where
  toRose (Node2 a b) = Rose "Node2" [toRose a, toRose b]
  toRose (Node3 a b c) = Rose "Node3" [toRose a, toRose b, toRose c]

-- | Treeを描画用のRoseへ変換する。Succは1段深くするだけなので子へ潜る。
-- toListと同様に再帰の型が`Tree (Node a)`へ変わる多相再帰なので型定義が必須。
treeToRose :: (ToRose a) => Tree a -> Rose
treeToRose (Zero a) = toRose a
treeToRose (Succ t) = treeToRose t

-- | Roseをツリー状の文字列へ整形する。末尾の子は └── 、それ以外は ├── で描く
renderRose :: Rose -> String
renderRose root = unlines $ go root
  where
    go :: Rose -> [String]
    go (Rose label children) = label : concatMap drawChild (markLast children)

    -- 末尾の子かどうかを判定するためにフラグを付ける
    markLast :: [Rose] -> [(Bool, Rose)]
    markLast [] = []
    markLast [x] = [(True, x)]
    markLast (x : xs) = (False, x) : markLast xs

    drawChild :: (Bool, Rose) -> [String]
    drawChild (isLast, child) =
      let (connector, contPrefix) =
            if isLast then ("└── ", "    ") else ("├── ", "│   ")
       in case go child of
            [] -> []
            (h : rest) -> (connector ++ h) : map (\line -> contPrefix ++ line) rest

main :: IO ()
main = do
  -- 高さ0: 要素1つだけの木
  let t0 = Zero 1 :: Tree Int
  putStrLn "-----t0-----"
  printTree t0
  -- 高さ1: 1つのNode2を持つ木 (要素2つ)
  let t1 = Succ (Zero (Node2 1 2)) :: Tree Int
  putStrLn "-----t1-----"
  printTree t1
  -- 高さ2: NodeのNodeを持つ木 (要素5つ)
  let t2 = Succ (Succ (Zero (Node2 (Node2 1 2) (Node3 3 4 5)))) :: Tree Int
  putStrLn "-----t2-----"
  printTree t2
  -- 高さ3: 根をNode3にした木を1段深くしたもの (要素12個)
  let t3 = Succ (Succ (Succ (Zero (Node3 (Node2 (Node2 1 2) (Node3 3 4 5)) (Node2 (Node3 6 7 8) (Node2 9 10)) (Node2 (Node2 11 12) (Node2 13 14)))))) :: Tree Int
  putStrLn "-----t3-----"
  printTree t3
