data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree :: Tree Int
tree = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                (Branch 3 Empty Empty)


leaves :: Tree a -> [a]
leaves Empty                  = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r)         = leaves l ++ leaves r
