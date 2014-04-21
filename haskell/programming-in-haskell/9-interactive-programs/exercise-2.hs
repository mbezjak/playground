import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import System.IO

token :: Parser a -> Parser a
token p = do
  spaces
  v <- p
  spaces
  return v

symbol :: String -> Parser String
symbol xs = Main.token (string xs)

nat :: Parser Int
nat = fmap read (many1 digit)

natural :: Parser Int
natural = Main.token nat

operate :: (Int -> Int) -> String -> Parser Int -> Parser Int
operate f s p = do
  symbol s
  v <- p
  return (f v)

expr :: Parser Int
expr = do t <- term
          operate (t+) "+" expr <|>
            operate (t-) "-" expr <|>
            return t

term :: Parser Int
term = do f <- factor
          operate (f*) "*" term <|>
            operate (f `div`) "/" term <|>
            return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> natural



beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

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

box :: [String]
box = ["+---------------+"
      ,"|               |"
      ,"+---+---+---+---+"
      ,"| q | c | d | = |"
      ,"+---+---+---+---+"
      ,"| 1 | 2 | 3 | + |"
      ,"+---+---+---+---+"
      ,"| 4 | 5 | 6 | - |"
      ,"+---+---+---+---+"
      ,"| 7 | 8 | 9 | * |"
      ,"+---+---+---+---+"
      ,"| 0 | ( | ) | / |"
      ,"+---+---+---+---+"]

buttons :: [Char]
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeat (1,y) xs | (y,xs) <- zip [1..] box]

display :: String -> IO ()
display xs = do writeat (3,2) "             "
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getChar
             if c `elem` buttons
               then process c xs
               else do beep
                       calc xs

process :: Char -> String -> IO ()
process c xs
  | c `elem` "qQ\ESC"    = quit
  | c `elem` "dD\BS\DEL" = delete xs
  | c `elem` "=\n"       = eval xs
  | c `elem` "cC"        = clear
  | otherwise            = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs =
  case parse expr "" xs of
    Right n  -> do clearerror
                   calc (show n)
    Left err -> do beep
                   showerror err
                   calc xs

clear :: IO ()
clear = calc ""

showerror :: ParseError -> IO ()
showerror err = writeat (1,14) (show err)

-- erases the screen from the current line down to the bottom of the screen
-- http://www.termsys.demon.co.uk/vtansi.htm
clearerror :: IO ()
clearerror = writeat (1,14) "\ESC[J"

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering
  hSetEcho stdin False
  run
