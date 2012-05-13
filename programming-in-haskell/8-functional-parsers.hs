import Text.ParserCombinators.Parsec

-- 1.
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

minus :: Parser String
minus = symbol "-"

int :: Parser Int
int = do minus
         n <- natural
         return (-n)
      <|> natural

-- 2.
comment :: Parser ()
comment = do symbol "--"
             many (noneOf "\n")
             optional newline

-- 3.
-- expr   ::= expr + expr | term
-- term   ::= term * term | factor
-- factor ::= (expr) | nat
-- nat    ::= 0 | 1 | 2 | ...
--
-- expr = "2 + 3 + 4"
--
-- parse tree #1:
--                   expr
--      expr          +          expr
--      term                expr  +  expr
--     factor               term     term
--      nat                factor   factor
--       2                  nat      nat
--                           3        4
--
-- parse tree #2:
--                   expr
--      expr          +          expr
-- expr  +  expr                 term
-- term     term                factor
--factor   factor                nat
-- nat      nat                   4
--  2        3



-- 4.
-- expr   ::= term + expr | term
-- term   ::= factor * term | factor
-- factor ::= (expr) | nat
-- nat    ::= 0 | 1 | 2 | ...
--
-- expr1 = "2 + 3"
--
--                   expr
--           term     +    expr
--          factor         term
--           nat          factor
--            2            nat
--                          3
--
--
-- expr1 = "2 * 3 * 4"
--
--                   expr
--                   term
--     factor          *         term
--      nat               factor  *  term
--       2                 nat      factor
--                          3        nat
--                                    4
--
--
-- expr1 = "(2 + 3) + 4"
--
--                   expr
--      term          +          expr
--     factor                    term
--   (  expr  )                 factor
-- term  +  expr                 nat
--factor    term                  4
-- nat     factor
--  2       nat
--           3



-- 5.
-- For `expr' productions and in some cases, parser would need to match
-- arbitrary complex `term' only to find out that string doesn't continue with
-- `+' symbol, then backtrack and do the same job all over again - matching the
-- same (complex) `term'.
--
-- E.g. for expr = "42", parse would look like:
-- expr
--   -> term + expr
--   -> factor * term + expr
--   -> (expr) * term + expr
--   (no match; backtrack to factor #2)
--   -> nat * term + expr
--   -> 42 * term + expr
--   (no match; backtrack to term #2)
--   -> factor + expr
--   -> (expr) + expr
--   (no match; backtrack to factor #2)
--   -> nat + expr
--   -> 42 + expr
--   (no match; backtrack to expr #2)
--   -> term
--   -> factor * term
--   -> (expr) * term
--   (no match; backtrack to factor #2)
--   -> nat * term
--   -> 42 * term
--   (no match; backtrack to term #2)
--   -> factor
--   -> (expr)
--   (no match; backtrack to factor #2)
--   -> nat
--   -> 42
--   (match)
--
-- To contrast, with simplified grammar and expr = "42", parse would look like:
-- expr
--   -> term (+ expr | 𝛆)
--   -> factor (* term | 𝛆) (+ expr | 𝛆)
--   -> (expr) (* term | 𝛆) (+ expr | 𝛆)
--   (no match; backtrack to factor #2)
--   -> nat (* term | 𝛆) (+ expr | 𝛆)
--   -> 42 (* term | 𝛆) (+ expr | 𝛆)
--   -> 42 (+ expr | 𝛆)
--   -> 42
--   (match)



-- 6.
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

-- 7.
expr2 :: Parser Int
expr2 = do t <- term2
           operate (t+) "+" expr2 <|>
             operate (t-) "-" expr2 <|>
             return t

term2 :: Parser Int
term2 = do f <- expo2
           operate (f*) "*" term2 <|>
             operate (f `div`) "/" term2 <|>
             return f

expo2 :: Parser Int
expo2 = do f <- factor2
           operate (f^) "^" expo2 <|>
             return f

factor2 :: Parser Int
factor2 = do symbol "("
             e <- expr2
             symbol ")"
             return e
           <|> natural

-- 8.
-- a)
-- expr ::= expr - nat | nat

-- b)
expr3 :: Parser Int
expr3 = do e <- expr3
           symbol "-"
           n <- natural
           return (e - n)
         <|> natural

-- c)
-- Grammar is now left-recursive and top-down parser can't handle left recursion

-- d)
expr4 :: Parser Int
expr4 = do n <- natural
           do xs <- many $ do { symbol "-"; natural }
              return $ foldl (-) n xs
            <|> return n
