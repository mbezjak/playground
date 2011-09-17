cylinder :: RealFloat a => a -> a -> a
cylinder r h =
  let side = 2 * pi * r * h
      top  = pi * r ^ 2
  in  side + 2 * top

calcBmis :: RealFloat a => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

fats :: RealFloat a => [(a, a)] -> [a]
fats xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

head'       :: [a] -> a
head' []    = error "No head for empty list!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of []    -> error "No head for empty list!"
                       (x:_) -> x

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty"
                                               [x] -> "singleton"
                                               xs  -> "really long"

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where what []  = "empty"
        what [x] = "singleton"
        what xs  = "longer"
