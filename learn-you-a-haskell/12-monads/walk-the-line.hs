import Control.Monad

type Birds = Int
type Pole  = (Birds,Birds)

landLeft :: Birds -> Pole -> Pole
landLeft n (left,right) = (left + n,right)

landRight :: Birds -> Pole -> Pole
landRight n (left,right) = (left,right + n)

(-:) :: a -> (a -> b) -> b
x -: f = f x

landLeft' :: Birds -> Pole -> Maybe Pole
landLeft' n (left,right)
  | abs ((left + n) - right) <= 3 = Just (left + n,right)
  | otherwise                     = Nothing

landRight' :: Birds -> Pole -> Maybe Pole
landRight' n (left,right)
  | abs (left - (right + n)) <= 3 = Just (left,right + n)
  | otherwise                     = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine :: Maybe Pole
routine = case landLeft' 1 (0,0) of
  Nothing -> Nothing
  Just pole1 -> case landRight' 4 pole1 of
    Nothing -> Nothing
    Just pole2 -> case landLeft' 2 pole2 of
      Nothing -> Nothing
      Just pole3 -> landLeft' 1 pole3

routine' :: Maybe Pole
routine' = do
  start <- return (0,0)
  first <- landLeft' 2 start
  second <- landRight' 2 first
  landLeft' 1 second

routine'' :: Maybe Pole
routine'' = do
  start <- return (0,0)
  first <- landLeft' 2 start
  Nothing
  second <- landRight' 2 first
  landLeft' 1 second
