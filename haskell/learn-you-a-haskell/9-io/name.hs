main = do
  putStrLn "Hello, what's yout name?"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!")
