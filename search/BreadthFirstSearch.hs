import Data.List (foldl')
import Data.Map qualified as Map
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set

type Graph = Map.Map Int [Int]

-- | (頂点, 隣接頂点リスト)のリストからグラフを構築する。
buildGraph :: [(Int, [Int])] -> Graph
buildGraph adjacency = Map.fromList adjacency

-- | startから到達可能な頂点を幅優先で訪問し、訪問順のリストを返す。
-- depthFirstSearchFunc.hsと同じく「これから訪問する頂点のワークリスト」を引数に持つ単一再帰で実装する。
-- 重複排除はポップ時のSet.memberチェックのみで行うため、同じ頂点がワークリストに複数回積まれうる。
bfs :: Graph -> Int -> [Int]
bfs graph start = go Set.empty (Seq.singleton start)
  where
    -- visited: 訪問済みの頂点の集合
    -- queue: これから処理する頂点のワークリスト(先頭から取り出す)
    go :: Set.Set Int -> Seq Int -> [Int]
    go _ Seq.Empty = [] -- ワークリストが空なら終了
    go visited (v Seq.:<| rest)
      | Set.member v visited = go visited rest -- ポップ時に訪問済みなら捨てる
      | otherwise =
          let neighbors = Map.findWithDefault [] v graph
              -- 隣接頂点をキュー末尾(FIFO)へ積む
              -- 先頭へ積めば深さ優先(depthFirstSearchFunc.hs)になる。
              -- NOTE: Listの++で連結すると計算量がO(length rest)になり、dfsと比べてコストがかさむため、Data.Sequencesを使い、O(1)で連結している
              queue' = foldl' (|>) rest neighbors
           in v : go (Set.insert v visited) queue'

main :: IO ()
main = do
  -- サンプルグラフ:
  --   1 -> 2, 3
  --   2 -> 4
  --   3 -> 4, 5
  --   4 -> 6
  --   5 -> 6
  let graph =
        buildGraph
          [ (1, [2, 3]),
            (2, [4]),
            (3, [4, 5]),
            (4, [6]),
            (5, [6]),
            (6, [])
          ]
  print $ graph
  print $ bfs graph 1 -- [1,2,3,4,5,6]
