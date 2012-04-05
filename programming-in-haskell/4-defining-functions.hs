-- 1.
halve :: [a] -> ([a], [a])
halve xs = splitAt mid xs
  where mid = (length xs) `div` 2

-- 2. a)
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then [] else drop 1 xs

-- 2. b)
safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = drop 1 xs

-- 2. c)
safetail3 :: [a] -> [a]
safetail3 []     = []
safetail3 (_:xs) = xs

-- 3.
{- a)
True  ∨ True  = True
True  ∨ False = True
False ∨ True  = True
False ∨ False = False
-}

{- b)
False ∨ False = False
_     ∨ _     = True
-}

{- c)
False ∨ b = b
True  ∨ _ = True
-}

{- d)
a ∨ b | a == b    = a
      | otherwise = True
-}

-- 4.
--a ∧ b = if a == True && b == True then True else False

-- 5.
--a ∧ b = if a == True then b else False

-- 6.
mult = \x -> \y -> \z -> x * y * z
