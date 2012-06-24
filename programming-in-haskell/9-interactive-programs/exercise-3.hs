import Data.List
import System.IO
import Control.Concurrent

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x,y) =
  putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

width :: Int
width = 5

height :: Int
height = 5

type Pos = (Int,Int)
type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: Board -> IO ()
showcells b = seqn [writeat p "O" | p <- b]

showborders :: IO ()
showborders = do
  vertical
  horizontal
    where vertical = seqn [writeat (width+1,n) "|" | n <- [1..height]]
          horizontal = seqn [writeat (n,height+1) "-" | n <- [1..width+1]]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1)
                         ,(x+1,y-1),(x-1,y)
                         ,(x+1,y),(x-1,y+1)
                         ,(x,y+1),(x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width)+1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [(x,y) | x <- [1..width],
                    y <- [1..height],
                    isEmpty b (x,y),
                    liveneighbs b (x,y) == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x:rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showborders
            partialUpdate [] b

remove :: Board -> IO ()
remove b = seqn [writeat p " " | p <- b]

partialUpdate :: Board -> Board -> IO ()
partialUpdate prev current = do
  remove dead
  showcells new
  wait 1000
  partialUpdate current (nextgen current)
    where dead = prev \\ current
          new  = current \\ prev

wait :: Int -> IO ()
wait n = threadDelay $ n * 1000

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering
  hSetEcho stdin False
  life glider
