# Binary Search

## 値の探索

- [binary_search/binarySearch.hs](./binarySearch.hs)
- 単純な二分探索を使い、値の存在するかを探す

## めぐる式二分探索

- [binary_search/binarySearchMeguru.hs](./binarySearchMeguru.hs)
- ある値が存在するかを調べるのではなく、ある条件を満たす最小/最大の値を見つけるために探索範囲を、条件を満たさない値(`ng`)と条件を満たす値(`ok`)の境界で絞り、`abs(ok - ng) == 1`になるまで狭めて境界`ok`を返す。
  - 参考: [二分探索アルゴリズムを一般化 〜 めぐる式二分探索法のススメ 〜](https://qiita.com/drken/items/97e37dd6143e33a64c8c)
  - 参考: [めぐる式二分探索本家](https://x.com/meguru_comp/status/697008509376835584)

### 応用例: 関数を満たす最大/最小の値を二分探索で探す

- [binary_search/binarySearchFunc.hs](./binarySearchFunc.hs)
- めぐる式二分探索を使い、関数f(x)がピボットに対してTrueかFalseかを判定することで、関数f(x)を満たす最大/最小のxの範囲を探索できる。
