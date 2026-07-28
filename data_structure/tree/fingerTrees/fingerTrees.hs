-- 以下のdata型の定義はhttps://www.staff.city.ac.uk/~ross/papers/FingerTree.htmlに記載のあるものを流用している。
data FingerTree a
  = Empty -- 要素なし
  | Single a -- 要素1つ
  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
  deriving (Show)

type Digit a = [a] -- 端の1〜4個

-- \| 2-3木と同じものを流用
data Node a = Node2 a a | Node3 a a a
  deriving (Show)

main :: IO ()
main = do
  print $ (Single 1 :: FingerTree Int)
  print $
    Deep
      [1, 2]
      ( Deep
          [Node2 3 4] -- 中心の中身は Node Int
          (Single (Node2 (Node2 5 6) (Node3 7 8 9))) -- さらに中心は Node(Node Int)
          [Node3 10 11 12]
      )
      [13, 14]
