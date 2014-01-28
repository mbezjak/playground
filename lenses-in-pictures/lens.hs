{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Point = Point { _x, _y :: Integer }
data Mario = Mario { _location :: Point }

instance Show Point where
  show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Show Mario where
  show (Mario p) = "Mario" ++ show p

player1 = Mario (Point 0 0)



moveX (Mario (Point x y)) val = Mario (Point (x+val) y)

useManual = moveX player1 10

-- or

makeLenses ''Point
makeLenses ''Mario

useLens = print (location.x +~ 10 $ player1)
