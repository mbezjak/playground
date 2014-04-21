import Data.Char

main = do
  putStrLn "What's your first name?"
  firstname <- getLine
  putStrLn "What's your last name?"
  lastname <- getLine
  let bigFirstname = map toUpper firstname
      bigLastname  = map toUpper lastname
  putStrLn $ "hey " ++ bigFirstname ++ " " ++ bigLastname ++ ", how are you?"
