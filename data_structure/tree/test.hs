data Set a
  = Tip -- 空
  | Bin Int a (Set a) (Set a) -- 部分木に含まれる総数 このツリーの要素 左の枝 右の枝
  deriving (Show)

empty = Tip :: Set Int

main :: IO ()
main = do
  print empty
  print Tip
