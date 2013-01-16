import Text.Printf
import Control.Parallel

-- tested on:
--   cpu = i5, 3.33 GHz, 4MB cache
--   mem = 8 GB

-- about 9s
main = (a::Integer) `par` (b::Integer) `par` (c::Integer) `pseq`
       (printf "A = %d\nB = %d\nC = %d\n" a b c) where
         a = ack 3 11 -- about 5s
         b = fac 50   -- fast no matter what
         c = fib 40   -- about 8s

fac 0 = 1
fac n = n * fac (n-1)

ack 0 n = n + 1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
