import Data.List (foldl')
import Prelude hiding ((++))

-- O(lenght xs)
(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs

-- O(1)
head' :: [a] -> a
head' (x : _) = x

-- O(1)
tail' :: [a] -> [a]
tail' (_ : xs) = xs

-- O(n)
last' :: [a] -> a
last' [] = error "empty list"
last' [x] = x
last' (_ : xs) = last' xs

-- O(n)
init' :: [a] -> [a]
init' [] = error "empty list"
init' [x] = []
init' (x : xs) = x : init' xs

-- flip (:)でacc x -> x : accとなり、[1,2,3]は、3:2:1:[]と展開されるO(n)
-- ghci> 1 : []
-- [1]
-- ghci> (:) 1 []
-- [1]
-- ghci> flip (:) [] 1
-- [1]
-- ghci> flip (:) [1] 2
-- [2,1]
-- NOTE: foldr (\x acc -> acc ++ [x]) [] asのように実装すると、末尾連結が必要なため、O(n^2)になってしまう
reverse' :: [a] -> [a]
reverse' as = foldl' (flip (:)) [] as

-- foldr (\_ acc -> acc + 1) 0 xsのように実装しても+は正格な演算子なので遅延の強みがでない。
length' :: [a] -> Int
length' xs = foldl' (\acc _ -> acc + 1) 0 xs

main :: IO ()
main = do
  print ([] ++ [3 .. 20])
  print ([1, 2] ++ [3 .. 20]) -- O(2)
  print ([1 .. 20] ++ [21, 22]) -- O(20)
  print $ init' [1 .. 20] -- O(20)
  print $ last' [1] -- O(20)
  print $ head' [1, 2, 3] -- O(1)
  print $ tail' [1, 2, 3] -- O(1)
  print $ reverse [1 .. 20]

  print $ length [1 .. 20] -- O(20)
