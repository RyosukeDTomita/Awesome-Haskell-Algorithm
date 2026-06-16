# Depth First Search

## ワークリスト版

- [depthFirstSearchFunc.hs](./depthFirstSearchFunc.hs)
- 基本的にこっちを使うでいいと思っている。
- これから訪問する頂点のワークリスト(スタック)を引数に持つ単一再帰でシンプルに実装
- 隣接頂点をスタック先頭(LIFO)へ積むと深さ優先、末尾へ積むと幅優先探索になる(訪問順は一致する)。
  - 本リポジトリの[BreadthFirstSearch.hs](../BreadthFirstSearch.hs)はまさにこの素朴な積み替え版で、ワークリストの末尾へ積むだけのFIFO実装になっている。
- `v : go ...` で遅延的に列挙するため O(n) で、実装も単純。

---

## タプル畳み込み版

- [depthFirstSearch.hs](./depthFirstSearch.hs)
- 頂点を訪問する `visit` を構造的に再帰させ、`(訪問済みSet, 訪問順リスト)` のタプルを畳み込みで引き回す。
- 再帰構造が探索木と一致するため、帰りがけ順(postorder)や部分木の情報を取りやすく、訪問済みSetを戻り値に持つのが特徴
- ただし訪問順の連結に `++` を使うため計算量は O(n^2)。

---
