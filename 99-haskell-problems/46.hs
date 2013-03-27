and' :: Bool -> Bool -> Bool
and' True b = b
and' _ _    = False

or' :: Bool -> Bool -> Bool
or' False b = b
or' _ _     = True

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

not' :: Bool -> Bool
not' True  = False
not' False = True

table :: (Bool -> Bool -> Bool) -> [(Bool,Bool,Bool)]
table f = [ (a,b,f a b) | (a,b) <- truth2 ]

printTable :: (Bool -> Bool -> Bool) -> IO ()
printTable f = mapM_ (putStrLn . show) (table f)

truth2 :: [(Bool,Bool)]
truth2 = [ (False, False)
         , (False, True)
         , (True,  False)
         , (True,  True) ]
