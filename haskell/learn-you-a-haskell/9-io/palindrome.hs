main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map outputPalindromes . lines
  where
    outputPalindromes xs = if isPalindrome xs then "palindrome" else "not a palindrome"
    isPalindrome xs = xs == reverse xs
