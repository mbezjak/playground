def isPrime(n: Int): Boolean =
  List.range(2, n) forall (x => n % x != 0)

def primePairs(n: Int): List[(Int,Int)] =
  for { i <- List.range(1, n)
        j <- List.range(1, i)
        if isPrime(i+j) } yield (i, j)
