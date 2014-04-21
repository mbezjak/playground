import Data.List  (intersect)
import Data.Maybe (fromMaybe)

data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

type LayoutTree a = Tree (a, (Int, Int))


layout :: Tree a -> LayoutTree a
layout = grid 1
  where
    grid _ Empty                  = Empty
    grid h (Branch a Empty Empty) = Branch (a, (1, h)) Empty Empty
    grid h (Branch a l r)         =
      let h' = h + 1
          l' = grid h' l
          r' = grid h' r
      in join a h l' r' 1

coord :: LayoutTree a -> [(Int, Int)]
coord Empty                   = []
coord (Branch (_, (p,h)) l r) = (p,h):(coord l ++ coord r)

posX :: LayoutTree a -> Maybe Int
posX Empty                    = Nothing
posX (Branch (_, (p, _)) _ _) = Just p

moveRightBy :: Int -> LayoutTree a -> LayoutTree a
moveRightBy _ Empty                   = Empty
moveRightBy c (Branch (a, (p,h)) l r) = Branch (a, (p+c,h)) (moveRightBy c l) (moveRightBy c r)

moveTo :: LayoutTree a -> Int -> LayoutTree a
moveTo Empty _                     = Empty
moveTo t@(Branch (_, (p,_)) _ _) m = moveRightBy (m-p) t

join :: a -> Int -> LayoutTree a -> LayoutTree a -> Int -> LayoutTree a
join a h l r d =
  if null overlap then Branch (a, (post,h)) l r'
  else join a h l r (d+1)
  where r' = moveTo r posr
        overlap = (coord l) `intersect` (coord r')
        posl = fromMaybe 0 (posX l)
        post = posl + d
        posr = post + d


a = Empty

b = Branch 'b' Empty Empty

c = Branch 'c'
      (Branch 'l' Empty Empty)
      Empty

d = Branch 'd'
      Empty
      (Branch 'r' Empty Empty)

e = Branch 'e'
      (Branch 'l' Empty Empty)
      (Branch 'r' Empty Empty)

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )
