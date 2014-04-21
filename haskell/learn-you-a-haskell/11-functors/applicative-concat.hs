import Control.Applicative

main = do
  a <- (++) <$> getLine <*> getLine
  putStrLn $ "Concatenated lines: " ++ a
