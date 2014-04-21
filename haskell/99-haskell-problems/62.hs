data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree :: Tree Int
tree = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                (Branch 3 Empty Empty)


internals :: Tree a -> [a]
internals Empty                  = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r)         = x:(internals l ++ internals r)
