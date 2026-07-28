# 数学ぽいやつ

## ユークリッドの互助法

[euclidean](./euclidean.hs)

---

## 素数関連アルゴリズム

[prime_number](./prime_number/)

---

## フェルマーの小定理

フェルマーの小定理を利用して法的逆元(逆数)を求めるアルゴリズム

a^(p-1) ≡ 1 (mod p) であることから、
a * a^(p-2) ≡ 1 (mod p) となり、a^(p-2) が a の法的逆元となる。

[fermat](./fermat.hs)

---

## 円周率の多角形近似

[pi.hs](./pi.hs)

---

## n進数

- [baseN.hs](./baseN.hs)
- 10進数からn進数に変換: `foldl'`で畳み込む
- n進数から10進数に戻す: `unfoldr`で展開

---

## ニュートン法

平方根をニュートンラフソン法で求めるアルゴリズムは別ディレクトリに実装済み。

[4_1newtonRoot.hs](../WhyFunctionalProgrammingMatters/4_1newtonRoot.hs)
