import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.List (foldl')
import Debug.Trace (traceShowId)

type Graph = Map.Map Int [Int]


-- (頂点, 隣接頂点リスト)のリストからグラフを構築する。
buildGraph :: [(Int, [Int])] -> Graph
buildGraph adjacency = Map.fromList adjacency

-- startから到達可能な頂点を深さ優先で訪問し、訪問順のリストを返す。
dfs :: Graph -> Int -> [Int]
dfs graph start = snd $ visit graph Set.empty start

-- 頂点vを訪問する。
-- visited: 訪問済みの頂点の集合
visit :: Graph -> Set.Set Int -> Int -> (Set.Set Int, [Int])
visit graph visited v
  | Set.member v visited = (visited, []) -- 訪問済み
  | otherwise =
      let visited' = Set.insert v visited
          -- !neighbors = traceShowId $ Map.findWithDefault [] v graph
          neighbors = Map.findWithDefault [] v graph -- 頂点vがキーになっているMapを探す。ないなら[]
          (visitedFinal, restOrder) = foldl' step (visited', []) neighbors -- 2からいけるのは2 -> 4 -> 6までその後、3から行けるのは3 -> 5なので深さ優先での訪問順は1 -> 2 -> 4 -> 6 -> 3 -> 5になる
       in (visitedFinal, v : restOrder) -- 自分を先に訪問しているはずなので冒頭に追加
  where
    -- visited: 訪問済みSet
    -- orderAcc: 訪問順のリスト
    step :: (Set.Set Int, [Int]) -> Int -> (Set.Set Int, [Int])
    step (visited, orderAcc) next =
      let (visited', order) = visit graph visited next -- 頂点nextを訪問する
       in (visited', orderAcc ++ order)

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
