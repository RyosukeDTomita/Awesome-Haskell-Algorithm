# Tree

## 平衡二分探索木 (Set: balanced binary tree)

- [Set.hs](Set.hs)
- (部分木に含まれる総数, このツリーの要素, 左の枝, 右の枝)というデータ構造を持つのが特徴
- 挿入、削除のタイミングで4種類の回転を行うことで木の平衡を保つ。この際に部分木に含まれる総数をデータ構造として持っていることが効いてくる
  - 平衡が崩れると木が深くなり、探索がO(n)に近づいてしまう。
  - 平衡な場合、木の深さはlog2 nになるため、探索コストがO(log2 n)となる。
- Reference:
  - [Functional Pearls Efficient sets—a balancing act](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/functional-pearls-efficient-setsa-balancing-act/0CAA1C189B4F7C15CE9B8C02D0D4B54E)
  - [Haskell Data.Set.Internal](https://hackage-content.haskell.org/package/containers-0.8/docs/src/Data.Set.Internal.html)

---

## finger tree (Seq)

---

## 一般的な二分木

- [tree.hs](./tree.hs)
