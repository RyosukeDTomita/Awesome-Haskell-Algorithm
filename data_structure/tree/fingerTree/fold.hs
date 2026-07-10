import Data.Foldable (fold)
import Data.Monoid (Sum (..))

main :: IO ()
main = do
  -- foldはfoldMapでできていて、foldMapはfoldrでできてそうに見える
  let sumList = fold [1, 2, 3 :: Sum Int]
  print sumList -- Sum {getSum = 6}
