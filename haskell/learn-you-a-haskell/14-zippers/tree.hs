data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node 'P'
      (Node 'O'
           (Node 'L'
                (Node 'N' Empty Empty)
                (Node 'T' Empty Empty)
           )
           (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
           )
      )
      (Node 'L'
           (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
           )
           (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
           )
      )

changeToP :: Tree Char -> Tree Char
changeToP (Node x l (Node y (Node _ m n) r)) = Node x l (Node y (Node 'P' m n) r)


data Direction = L | R deriving (Show)
type Directions = [Direction]

changeToP' :: Directions -> Tree Char -> Tree Char
changeToP' [] (Node _ l r) = Node 'P' l r
changeToP' (L:ds) (Node x l r) = Node x (changeToP' ds l) r
changeToP' (R:ds) (Node x l r) = Node x l (changeToP' ds r)

elemAt :: Directions -> Tree a -> a
elemAt [] (Node x _ _) = x
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r


type Breadcrumbs = [Direction]

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

x -: f = f x


data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs' a = [Crumb a]

goLeft' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goLeft' (Node x l r, bs) = (l,LeftCrumb x r:bs)

goRight' :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goRight' (Node x l r, bs) = (r,RightCrumb x l:bs)

goUp :: (Tree a, Breadcrumbs' a) -> (Tree a, Breadcrumbs' a)
goUp (t, LeftCrumb x r:bs)  = (Node x t r,bs)
goUp (t, RightCrumb x l:bs) = (Node x l t,bs)


type Zipper a = (Tree a, Breadcrumbs' a)

modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs)      = (Empty, bs)

attach :: Tree a -> Zipper a -> Zipper a
attach t (_,bs) = (t,bs)

topMost :: Zipper a -> Zipper a
topMost (t,[]) = (t,[])
topMost z = topMost (goUp z)


-- with failure context:

goLeft3 :: Zipper a -> Maybe (Zipper a)
goLeft3 (Node x l r,bs) = Just (l,LeftCrumb x r:bs)
goLeft3 (Empty,_)       = Nothing

goRight3 :: Zipper a -> Maybe (Zipper a)
goRight3 (Node x l r,bs) = Just (r,RightCrumb x l:bs)
goRight3 (Empty,_)       = Nothing

goUp3 :: Zipper a -> Maybe (Zipper a)
goUp3 (t,LeftCrumb x r:bs)  = Just (Node x t r,bs)
goUp3 (t,RightCrumb x l:bs) = Just (Node x l t,bs)
goUp3 (t,[])                = Nothing
