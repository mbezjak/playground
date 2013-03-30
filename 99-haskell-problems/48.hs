and' :: Bool -> Bool -> Bool
and' True b = b
and' _ _    = False
infixl 3 `and'`

or' :: Bool -> Bool -> Bool
or' False b = b
or' _ _     = True
infixl 2 `or'`

nand' :: Bool -> Bool -> Bool
nand' a b = not' (and' a b)

nor' :: Bool -> Bool -> Bool
nor' a b = not' (or' a b)

xor' :: Bool -> Bool -> Bool
xor' a b = a /= b

impl' :: Bool -> Bool -> Bool
impl' a b = (not' a) `or'` b

equ' :: Bool -> Bool -> Bool
equ' a b = not' (xor' a b)
infixl 1 `equ'`

not' :: Bool -> Bool
not' True  = False
not' False = True
infixl 4 `not'`

tablen :: Int -> ([Bool] -> Bool) -> [[Bool]]
tablen n f = [ xs ++ [f xs] | xs <- truthn n ]

printTablen :: Int -> ([Bool] -> Bool) -> IO ()
printTablen n f = mapM_ (putStrLn . show) (tablen n f)

truth2 = [(True, False)]

truthn :: Int -> [[Bool]]
truthn 1 = [[False], [True]]
truthn n = [ x:xs | x <- [False, True], xs <-truthn (n-1) ]
