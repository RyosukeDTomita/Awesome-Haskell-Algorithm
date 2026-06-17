import Data.Map qualified as Map
import Data.Set qualified as Set

type Graph = Map.Map Int [Int]

-- | (頂点, 隣接頂点リスト)のリストからグラフを構築する。
buildGraph :: [(Int, [Int])] -> Graph
buildGraph adjacency = Map.fromList adjacency

-- | startから到達可能な頂点を深さ優先で訪問し、訪問順のリストを返す。
-- depthFirstSearch.hsのvisit/stepによるタプル畳み込み版と異なり、「これから訪問する頂点のワークリスト」を引数に持つ単一再帰で実装する。
dfs :: Graph -> Int -> [Int]
dfs graph start = go Set.empty [start]
  where
    -- visited: 訪問済みの頂点の集合
    -- stack: これから訪問する頂点のワークリスト(先頭が次に処理する頂点)
    go :: Set.Set Int -> [Int] -> [Int]
    go _ [] = [] -- ワークリストが空なら終了
    go visited (v : stack)
      | Set.member v visited = go visited stack
      | otherwise =
          let neighbors = Map.findWithDefault [] v graph
           in -- 隣接頂点をスタックの先頭(LIFO)へ積むのが深さ優先。
              -- 末尾へ積めば幅優先(BreadthFirstSearch.hs)になる。
              -- NOTE: neighbors ++ stackの場合、計算量はO(length neighbors)でそんなに大きくならないため、Data.Sequencesを使っていない。
              v : go (Set.insert v visited) (neighbors ++ stack) -- 訪問済みを検証せずに無条件にpushする

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
  print $ dfs graph 1 -- [1,2,4,6,3,5]
