def isPrime(n: Int): Boolean =
  List.range(2, n) forall (x => n % x != 0)

def sumPrimes(start: Int, end: Int): Int = {
  var i   = start
  var acc = 0

  while (i < end) {
    if (isPrime(i)) acc += i
    i += 1
  }

  acc
}

def sumPrimes2(start: Int, end: Int): Int =
  (List.range(start, end) filter isPrime).sum

def secondPrime(start: Int, end: Int): Int =
  List.range(start, end) filter isPrime apply 1


// ---- Streams

def srange(start: Int, end: Int): Stream[Int] = {
  if (start >= end) Stream.empty
  else Stream.cons(start, srange(start + 1, end))
}

def sprint[A](xs: Stream[A]) {
  if (!xs.isEmpty) {
    Console.println(xs.head)
    sprint(xs.tail)
  }
}

def secondPrime2(start: Int, end: Int): Int =
  (Stream.range(start, end) filter isPrime apply 1)
