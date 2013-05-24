data Tree a = Empty | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

countHbalTrees :: Int -> Int
countHbalTrees = length . hbalTreeNodes

hbalTreeNodes :: Int -> [Tree Char]
hbalTreeNodes n = concatMap (withNNodes n) (heights n)

withNNodes :: Int -> Int -> [Tree Char]
withNNodes 0 0 = [Empty]
withNNodes 1 1 = [mkLeaf]
withNNodes _ 0 = []
withNNodes _ 1 = []
withNNodes 0 _ = []
withNNodes n h = [mkNode l r |
                  (hl,hr) <- [(h-1,h-1), (h-2,h-1), (h-1,h-2)],
                  nl <- [(minNodes hl) .. (n-1)],
                  let nr = n - nl - 1,
                  l <- withNNodes nl hl,
                  r <- withNNodes nr hr]

-- slow as a snail for obvious reason
hbalTreeNodes' :: Int -> [Tree Char]
hbalTreeNodes' n = filter ((==n). count) . concatMap hbalTree $ heights n

heights :: Int -> [Int]
heights n = [(minHeight n) .. (maxHeight n)]

-- minimum number of nodes in a height-balanced binary tree of height `h'
minNodes :: Int -> Int
minNodes h = fibs !! (h+2) - 1

-- maximum number of nodes in a height-balanced binary tree of height `h'
maxNodes :: Int -> Int
maxNodes h = 2^h - 1

-- maximum height of a height-balanced binary tree with `n' nodes
maxHeight :: Int -> Int
maxHeight n = length (takeWhile (<= n+1) fibs) - 3

-- minimum height of a height-balanced binary tree with `n' nodes
minHeight :: Int -> Int
minHeight n = ceiling . logBase 2 . fromIntegral $ n+1

fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

count :: Tree Char -> Int
count Empty = 0
count (Branch _ l r) = 1 + count l + count r

hbalTree :: Int -> [Tree Char]
hbalTree 0 = [Empty]
hbalTree 1 = [mkLeaf]
hbalTree h = [mkNode l r |
              (hl,hr) <- [(h-1,h-1), (h-2,h-1), (h-1,h-2)],
              l <- hbalTree hl,
              r <- hbalTree hr]

mkLeaf :: Tree Char
mkLeaf = Branch 'x' Empty Empty

mkNode :: Tree Char -> Tree Char -> Tree Char
mkNode = Branch 'x'
