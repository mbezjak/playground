data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red    == Red    = True
  Green  == Green  = True
  Yellow == Yellow = True
  _      == _      = False

instance Show TrafficLight where
  show Red    = "Red light"
  show Green  = "Green light"
  show Yellow = "Yellow light"


data Maybe' a = Nothing' | Just' a deriving (Show)

instance (Eq m) => Eq (Maybe' m) where
  Just' x  == Just' y  = x == y
  Nothing' == Nothing' = True
  _        == _        = False

data NotEquals = NotEquals deriving (Show) -- but not Eq
