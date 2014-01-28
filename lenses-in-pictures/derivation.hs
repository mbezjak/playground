import Data.Traversable
import Control.Monad.Identity

data Point = Point { _x, _y :: Integer }
data Mario = Mario { _location :: Point }

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Show Mario where
  show (Mario p) = "Mario" ++ show p

player1 = Mario (Point 0 0)



-- derivation
double :: Int -> Maybe Int
double x = Just (x * 2)

test_traverse = traverse double [1, 2, 3]

fmapDefault' :: Traversable t => (a -> b) -> t a -> t b
fmapDefault' f = runIdentity . traverse (Identity . f)

test_fmapdefault = fmapDefault' (+1) [1, 2, 3]

over' :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over' l f = runIdentity . l (Identity . f)

type Setter s t a b = (a -> Identity b) -> s -> Identity t

over :: Setter s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
