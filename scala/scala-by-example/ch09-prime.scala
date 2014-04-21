def isPrime(n: Int): Boolean =
  List.range(2, n) forall (x => n % x != 0)

def primePairs(n: Int): List[(Int,Int)] =
  List.range(1, n).
    flatMap(i => List.range(1, i).map(j => (i,j))).
    filter(pair => isPrime(pair._1 + pair._2))
