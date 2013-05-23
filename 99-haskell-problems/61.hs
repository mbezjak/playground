data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree :: Tree Int
tree = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                (Branch 3 Empty Empty)


countLeaves :: Tree a -> Int
countLeaves Empty                  = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r)         = countLeaves l + countLeaves r
