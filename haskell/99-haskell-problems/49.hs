gray :: Int -> [String]
gray 1 = ["0", "1"]
gray n = map ('0':) up ++ map ('1':) (reverse up)
  where up = gray (n-1)
