{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Text.Printf         (printf)
import System.Process      (readProcess)
import Control.Monad.Error (MonadError, ErrorT, runErrorT, throwError, lift)

newtype Shell a = Shell { runShell :: ErrorT String IO a }
                deriving (Functor, Monad, MonadError String)

newtype Priv a  = Priv  { priv :: Shell a }
                deriving (Functor, Monad, MonadError String)

run :: String -> IO String
run xs = let (cmd:args) = words xs in readProcess cmd args ""

sh :: String -> Shell String
sh = Shell . lift . run

unsh :: Shell a -> IO a
unsh s = do
  eith <- runErrorT . runShell $ s
  case eith of
    Left err -> error $ "error: " ++ err
    Right a  -> return a

shPriv :: String -> Priv String
shPriv s = Priv $ sh ("sudo " ++ s)

readM :: String -> Shell Integer
readM s | [x] <- parse = return x
        | otherwise    = throwError (show s)
  where
    parse = [ x | (x,_) <- reads s ]

get :: String -> Shell Integer
get s = do
  v <- sh $ "./stub-sysctl " ++ s
  readM (parse v)
  where
    parse = init . tail . dropWhile (/= '=')

set :: String -> Integer -> Priv String
set s v =
  shPriv $ printf "./stub-sysctl -w %s %s" s (show v)

modify :: String -> (Integer -> Integer) -> Shell (Integer, Integer)
modify s f = do
  v <- get s
  let u = f v
  priv $ set s u
  return (v,u)

toggle :: Integer -> Integer
toggle v | v == 100  = 0
         | otherwise = 100

speed :: Integer -> Double
speed clock = fromIntegral clock / 1000

main :: IO ()
main = do
  (old, new) <- unsh $ modify "hw.setperf" toggle
  clock      <- unsh $ get "hw.cpuspeed"
  printf "cpu: %d -> %d\n" old new
  printf "clock: %f Ghz\n" (speed clock)
