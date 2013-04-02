import Data.List (sortBy, insertBy)
import Data.Ord  (comparing)

data Tree = Leaf (Char,Int) | Node Int Tree Tree
          deriving Show

weight :: Tree -> Int
weight (Leaf (_,w)) = w
weight (Node w _ _) = w

join :: Tree -> Tree -> Tree
join l r = Node (weight l + weight r) l r

consume :: [Tree] -> [Tree]
consume [t]      = [t]
consume (l:r:xs) = consume $ insertBy (comparing weight) (join l r) xs

encode :: Tree -> [(Char,String)]
encode (Leaf (s,_)) = [(s,"")]
encode (Node _ l r) = map (code '0') (encode l) ++ map (code '1') (encode r)
  where code c (s,cs) = (s,c:cs)

huffman :: [(Char,Int)] -> [(Char,String)]
huffman = encode . head . consume . map Leaf . sortBy (comparing snd)
