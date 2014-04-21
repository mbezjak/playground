import Control.Monad

main = do
  colors <- forM [1..4] collectColors
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are:"
  mapM putStrLn colors

  where collectColors a = do
          putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
          getLine
