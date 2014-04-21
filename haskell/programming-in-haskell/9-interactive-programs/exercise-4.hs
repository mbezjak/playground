import Data.List
import System.IO

beep :: IO ()
beep = putStr "\BEL"

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
survivors b = [p | p <- b, liveneighbs b p `elem` [2,3]]

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
            showkeys
            partialUpdate [] b

showlife :: Board -> Board -> IO ()
showlife prev current = do
  remove dead
  showcells new
    where dead = prev \\ current
          new  = current \\ prev

showkeys :: IO ()
showkeys = do
  goto (1,height+1+1) -- account for game and border height
  putStrLn "Press SPC to pause and resume the game"
  putStrLn "Use vim keys HJKL to move around"
  putStrLn "Press ENTER to toggle between life and no life"

remove :: Board -> IO ()
remove b = seqn [writeat p " " | p <- b]

partialUpdate :: Board -> Board -> IO ()
partialUpdate prev current = do
  showlife prev current
  handlekeys (partialUpdate current) (nextgen current)

handlekeys :: (Board -> IO ()) -> Board -> IO ()
handlekeys continue next = do
  available <- hWaitForInput stdin 1000 -- ms
  if available then
    do next' <- maybeEnterInteractive next
       continue next'
  else
    continue next

maybeEnterInteractive :: Board -> IO Board
maybeEnterInteractive next = do
  c <- getChar
  if c == ' ' then
    do goto (1,1)
       interactive (1,1) next
  else
    return next

interactive :: Pos -> Board -> IO Board
interactive pos next = do
  c <- getChar
  if c == ' ' then
    return next -- exit interactive
  else if c `elem` "hH" then
    do pos' <- moveleft pos
       interactive pos' next
  else if c `elem` "lL" then
    do pos' <- moveright pos
       interactive pos' next
  else if c `elem` "jJ" then
    do pos' <- movedown pos
       interactive pos' next
  else if c `elem` "kK" then
    do pos' <- moveup pos
       interactive pos' next
  else if c == '\n' then
    do let next' = toggle pos next
       showlife next next'
       goto pos
       interactive pos next'
  else
    do beep
       interactive pos next

moveleft :: Pos -> IO Pos
moveleft (x,y) = goto p >> return p
  where p  = (x',y)
        x' = ((x-1-1) `mod` width) + 1

moveright :: Pos -> IO Pos
moveright (x,y) = goto p >> return p
  where p  = (x',y)
        x' = ((x-1+1) `mod` width) + 1

moveup :: Pos -> IO Pos
moveup (x,y) = goto p >> return p
  where p  = (x,y')
        y' = ((y-1-1) `mod` height) + 1

movedown :: Pos -> IO Pos
movedown (x,y) = goto p >> return p
  where p  = (x,y')
        y' = ((y-1+1) `mod` height) + 1

toggle :: Pos -> Board -> Board
toggle p b | p `elem` b = delete p b
           | otherwise  = p:b

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering
  hSetEcho stdin False
  life glider
