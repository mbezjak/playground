readLine :: IO String
readLine = do
  x <- getChar
  if x == '\n' then
    return ""
  else if x == '\DEL' then
    do putStr "\ESC[1D"
       return ""
  else
    do xs <- readLine
       return (x:xs)

main :: IO ()
main = do
  line <- readLine
  print line
